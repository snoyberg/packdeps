{-# LANGUAGE QuasiQuotes, TypeFamilies, OverloadedStrings, MultiParamTypeClasses #-}
import Yesod
import Yesod.Helpers.AtomFeed
import Yesod.Helpers.Feed
import Distribution.PackDeps
import Data.Maybe
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Time
import Distribution.Package
import Distribution.Text
import Control.Arrow
import Distribution.Version (withinRange)
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as S8

data PD = PD Newest Reverses
type Handler = GHandler PD PD
mkYesod "PD" [$parseRoutes|
/ RootR GET
/feed FeedR GET
/feed/#String Feed2R GET
/feed/#String/#String/#String/#String Feed3R GET
/specific SpecificR GET
/feed/specific/#String SpecificFeedR GET

/reverse/#String ReverseR GET
|]
instance Yesod PD where approot _ = ""

getRootR = defaultLayout $ do
    setTitle "Hackage dependency monitor"
    addCassius [$cassius|body
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
    [$hamlet|\
<h1>Hackage Dependency Monitor
<form action="@{FeedR}">
    <input type="text" name="needle" required="" placeholder="Search string">
    <input type="submit" value="Check">
<h2>What is this?
<p>It can often get tedious to keep your package dependencies up-to-date. This tool is meant to alleviate a lot of the burden. It will automatically determine when an upper bound on a package prevents the usage of a newer version. For example, if foo depends on bar &gt;= 0.1 &amp;&amp; &lt; 0.2, and bar 0.2 exists, this tool will flag it.
<p>Enter a search string in the box above. It will find all packages containing that string in the package name, maintainer or author fields, and create an Atom feed for restrictive bounds. Simply add that URL to a news reader, and you're good to go!
<p>
    \All of the code is 
    <a href="http://github.com/snoyberg/packdeps">available on Github
    \. Additionally, there is a 
    <a href="http://hackage.haskell.org/package/packdeps">package on Hackage
    \ with the code powering this site both as a library and as an executable, so you can test code which is not uploaded to the public Hackage server.
<div id="footer">
    <a href="http://docs.yesodweb.com/">Powered by Yesod
|]

getDeps needle = do
    PD newest _ <- getYesod
    let descs = filterPackages needle newest
        go (_, _, AllNewest) = Nothing
        go (PackageName x, v, WontAccept y z) = Just ((x, v), (y, z))
        deps = reverse $ sortBy (comparing $ snd . snd)
             $ mapMaybe (go . checkDeps newest) descs
    return deps

getFeedR :: Handler RepHtml
getFeedR = do
    needle <- runFormGet' $ stringInput "needle"
    deps <- getDeps needle
    let title = "Newer dependencies for " ++ needle
    defaultLayout $ do
        setTitle $ string title
        addCassius [$cassius|body
    font-family: Arial,Helvetica,sans-serif
    width: 600px
    margin: 2em auto
h1
    text-align: center
p
    text-align: justify
table
    border-collapse: collapse
th, td
    border: 1px solid #999
    padding: 5px
h3
    margin: 20px 0 5px 0
|]
        let feedR = Feed2R needle
        atomLink feedR title
        [$hamlet|\
<h1>#{title}
<p>
    \The following are the packages which have restrictive upper bounds. You can also 
    <a href="@{feedR}">view this information as a news feed
    \ so you can get automatic updates in your feed reader of choice.
$if null deps
    <p>
        <b>All upper bounds are non-restrictive.
$else
    $forall d <- deps
        <h3>#{fst (fst d)}-#{display (snd (fst d))}
        <table>
            $forall p <- fst (snd d)
                <tr>
                    <th>#{fst p}
                    <td>#{snd p}
|]

getFeed2R needle = do
    deps <- getDeps needle
    now <- liftIO getCurrentTime
    newsFeed Feed
        { feedTitle = "Newer dependencies for " ++ needle
        , feedLinkSelf = Feed2R needle
        , feedLinkHome = RootR
        , feedUpdated = now
        , feedEntries = map go' deps
        , feedLanguage = "en"
        , feedDescription = string $ "Newer dependencies for " ++ needle
        }
  where
    go' ((name, version), (deps, time)) = FeedEntry
        { feedEntryLink = Feed3R needle name (display version) (show time)
        , feedEntryUpdated = time
        , feedEntryTitle = "Outdated dependencies for " ++ name ++ " " ++ display version
        , feedEntryContent = [$hamlet|\
<table border="1">
    $forall d <- deps
        <tr>
            <th>#{fst d}
            <td>#{snd d}
|]
        }

getFeed3R :: String -> String -> String -> String -> Handler ()
getFeed3R _ package _ _ =
    redirectString RedirectPermanent
  $ S8.pack
  $ "http://hackage.haskell.org/cgi-bin/hackage-scripts/package/" ++ package

main = do
    newest <- read `fmap` readFile "newest"
    warpDebug 3000 $ PD newest $ getReverses newest

getSpecificR :: Handler RepHtml
getSpecificR = do
    packages' <- lookupGetParams "package"
    PD newest _ <- getYesod
    let packages = map (id &&& flip getPackage newest) packages'
    let title = "Newer dependencies for your Hackage packages"
    let checkDeps' x =
            case checkDeps newest x of
                (_, _, AllNewest) -> Nothing
                (_, v, WontAccept cd _) -> Just (v, cd)
    defaultLayout $ do
        setTitle $ string title
        addCassius [$cassius|body
    font-family: Arial,Helvetica,sans-serif
    width: 600px
    margin: 2em auto
h1
    text-align: center
p
    text-align: justify
table
    border-collapse: collapse
th, td
    border: 1px solid #999
    padding: 5px
h3
    margin: 20px 0 5px 0
|]
        let feedR = SpecificFeedR $ unwords packages'
        atomLink feedR title
        [$hamlet|\
<h1>#{title}
<p>
    \The following are the packages which have restrictive upper bounds. You can also 
    <a href="@{feedR}">view this information as a news feed
    \ so you can get automatic updates in your feed reader of choice.
$forall p <- packages
    $maybe descinfo <- snd p
        $maybe x <- checkDeps' descinfo
            <h3>#{fst p}-#{display (fst x)}
            <table>
                $forall p <- snd x
                    <tr>
                        <th>#{fst p}
                        <td>#{snd p}
        $nothing
            <h3>#{fst p} up to date
    $nothing
        <p>Invalid package name: #{fst p}
|]

getSpecificFeedR packages' = do
    PD newest _ <- getYesod
    let descs = mapMaybe (flip getPackage newest) $ words packages'
    let go (_, _, AllNewest) = Nothing
        go (PackageName x, v, WontAccept y z) = Just ((x, v), (y, z))
        deps = reverse $ sortBy (comparing $ snd . snd)
             $ mapMaybe (go . checkDeps newest) descs
    now <- liftIO getCurrentTime
    newsFeed Feed
        { feedTitle = "Newer dependencies for Hackage packages"
        , feedLinkSelf = SpecificFeedR packages'
        , feedLinkHome = RootR
        , feedUpdated = now
        , feedEntries = map go' deps
        , feedLanguage = "en"
        , feedDescription = "Newer dependencies for Hackage packages"
        }
  where
    go' ((name, version), (deps, time)) = FeedEntry
        { feedEntryLink = Feed3R packages' name (display version) (show time)
        , feedEntryUpdated = time
        , feedEntryTitle = "Outdated dependencies for " ++ name ++ " " ++ display version
        , feedEntryContent = [$hamlet|\
<table border="1">
    $forall d <- deps
        <tr>
            <th>#{fst d}
            <td>#{snd d}
|]
        }

getReverseR :: String -> Handler RepHtml
getReverseR dep = do
    PD _ reverse <- getYesod
    (version, rels) <- maybe notFound return $ Map.lookup dep reverse
    defaultLayout $ do
        setTitle $ string $ "Reverse dependencies for " ++ dep
        addCassius [$cassius|table
    border-collapse: collapse
th, td
    border: 1px solid #333
    padding: 3px
|]
        addHtml [$hamlet|\
<h1>Reverse dependencies for #{dep} #{display version}
<table>
    <tr>
        <th>Package
        <th>Uses current version?
    $forall rel <- rels
        <tr>
            <td>#{fst rel}
            $if withinRange version (snd rel)
                <td>Yes
            $else
                <td style="color:#900">No (#{display (snd rel)})
|]
