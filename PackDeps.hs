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
    [$hamlet|
%form!action=@FeedR@
    Search string: $
    %input!type=text!name=needle!required
    %input!type=submit
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
