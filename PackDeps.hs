{-# LANGUAGE QuasiQuotes, TypeFamilies, OverloadedStrings #-}
import Yesod
import Yesod.Helpers.AtomFeed
import CheckDeps
import Data.Maybe
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Time
import Distribution.Package
import Distribution.Text

data PD = PD Newest
type Handler = GHandler PD PD
mkYesod "PD" [$parseRoutes|
/ RootR GET
/feed FeedR GET
/feed/#String Feed2R GET
/feed/#String/#String/#String/#String Feed3R GET
|]
instance Yesod PD where approot _ = ""

getRootR = defaultLayout $ do
    setTitle "Hackage dependency monitor"
    addStyle [$cassius|
body
    font-family: Arial,Helvetica,sans-serif
    width: 600px
    margin: 2em auto
    text-align: center
p
    text-align: justify
h2
    border-bottom: 2px solid #999
input[type=text]
    width: 400px
#footer
    margin-top: 15px
    border-top: 1px dashed #999
    padding-top: 10px
|]
    [$hamlet|
%h1 Hackage Dependency Monitor
%form!action=@FeedR@
    %input!type=text!name=needle!required!placeholder="Search string"
    %input!type=submit!value=Check
%h2 What is this?
%p It can often get tedious to keep your package dependencies up-to-date. This tool is meant to alleviate a lot of the burden. It will automatically determine when an upper bound on a package prevents the usage of a newer version. For example, if foo depends on bar &gt;= 0.1 &amp;&amp; &lt; 0.2, and bar 0.2 exists, this tool will flag it.
%p Enter a search string in the box above. It will find all packages containing that string in the package name, maintainer or author fields, and create an Atom feed for restrictive bounds. Simply add that URL to a news reader, and you're good to go!
%p
    All of the code is $
    %a!href="http://github.com/snoyberg/packdeps" available on Github
    \. Most likely in the near future I'll also publish an executable on Hackage which will let you do this check against non-published packages.
#footer
    %a!href="http://docs.yesodweb.com/" Powered by Yesod
|]

getFeedR :: Handler ()
getFeedR = do
    needle <- runFormGet' $ stringInput "needle"
    redirect RedirectPermanent $ Feed2R needle

getFeed2R needle = do
    PD newest <- getYesod
    let descs = filterPackages needle newest
        go (_, _, AllNewest) = Nothing
        go (x, v, WontAccept y z) = Just ((x, v), (y, z))
        deps = reverse $ sortBy (comparing $ snd . snd)
             $ mapMaybe (go . checkDeps newest) descs
    now <- liftIO getCurrentTime
    atomFeed AtomFeed
        { atomTitle = "Newer dependencies for " ++ needle
        , atomLinkSelf = Feed2R needle
        , atomLinkHome = RootR
        , atomUpdated = now
        , atomEntries = map go' deps
        }
  where
    go' ((PackageName name, version), (deps, time)) = AtomFeedEntry
        { atomEntryLink = Feed3R needle name (display version) (show time)
        , atomEntryUpdated = time
        , atomEntryTitle = "Outdated dependencies for " ++ name ++ " " ++ display version
        , atomEntryContent = [$hamlet|
%table!border=1
    $forall deps d
        %tr
            %th $fst.d$
            %td $snd.d$
|]
        }

getFeed3R :: String -> String -> String -> String -> Handler ()
getFeed3R _ package _ _ =
    redirectString RedirectPermanent
  $ "http://hackage.haskell.org/cgi-bin/hackage-scripts/package/" ++ package

main = do
    newest <- loadNewest "/home/snoyman/.cabal/packages/hackage.haskell.org/00-index.tar"
    basicHandler 3000 $ PD newest
