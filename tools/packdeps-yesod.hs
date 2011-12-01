{-# LANGUAGE QuasiQuotes, TypeFamilies, OverloadedStrings, MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
import Yesod
import Yesod.AtomFeed
import Yesod.Feed
import Distribution.PackDeps
import Data.Maybe
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Time
import Distribution.Package
import Distribution.Text hiding (Text)
import Control.Arrow
import Distribution.Version (withinRange)
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as S8
import Data.Text (Text, pack, unpack)
import Text.Hamlet (shamlet)
import System.Environment (getArgs)
import Data.List (sort)

data PD = PD Newest Reverses
mkYesod "PD" [$parseRoutes|
/favicon.ico FaviconR GET
/ RootR GET
/feed FeedR GET
/feed/#Text Feed2R GET
/feeddeep/#Text Feed2DeepR GET
/feed/#Text/#Text/#Text/#Text Feed3R GET
/specific SpecificR GET
/feed/specific/#Text SpecificFeedR GET

/reverse ReverseListR GET
/reverse/#Text ReverseR GET
|]
instance Yesod PD where approot _ = ""

getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "favicon.ico"

mainCassius = [$cassius|
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
table
    border-collapse: collapse
    margin: 0 auto
th, td
    border: 1px solid #333
form p
    margin-top: 1em
    text-align: center
|]

getRootR = defaultLayout $ do
    setTitle "Hackage dependency monitor"
    addCassius mainCassius
    [whamlet|
<h1>Hackage Dependency Monitor
<form action="@{FeedR}">
    <input type="text" name="needle" required="" placeholder="Search string">
    <input type="submit" value="Check">
    <p>
        <a href=@{ReverseListR}>Reverse Dependency List
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

isDeep = fmap (== Just "on") $ runInputGet $ iopt textField "deep"

getDeps deep needle = do
    PD newest _ <- getYesod
    let descs' = filterPackages needle newest
        descs = if deep then deepDeps newest descs' else descs'
        go (_, _, AllNewest) = Nothing
        go (PackageName x, v, WontAccept y z) = Just ((x, v), (y, z))
        deps = reverse $ sortBy (comparing $ snd . snd)
             $ mapMaybe (go . checkDeps newest) descs
    return (descs, deps)

instance RenderMessage PD FormMessage where
    renderMessage _ _ = defaultFormMessage

getFeedR :: Handler RepHtml
getFeedR = do
    needle <- runInputGet $ ireq textField "needle"
    deep <- isDeep
    (descs, deps) <- getDeps deep $ unpack needle
    let title = "Newer dependencies for " ++ unpack needle
    let deepR = (FeedR, [("needle", needle), ("deep", "on")])
    defaultLayout $ do
        setTitle $ toHtml title
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
.packages
    -webkit-column-count: 2
    -moz-column-count: 2
    column-count: 2
.packages a, .packages a:visited
    text-decoration: none
    color: blue
.packages a:hover
    text-decoration: underline
|]
        let feedR = (if deep then Feed2DeepR else Feed2R) needle
        atomLink feedR title
        [whamlet|
<h1>#{title}
<p>
    \The following are the packages which have restrictive upper bounds. You can also #
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
$if not deep
    <p>
        <a href=@?{deepR}>View outdated dependency for all ancestor packages too.
<h3>Packages checked
<div .packages>
    <ol>
        $forall name <- sort $ map diName descs
            <li>
                <a href="http://hackage.haskell.org/package/#{name}">#{name}
|]

getFeed2R needle = do
    (_, deps) <- getDeps False $ unpack needle
    feed2Helper needle deps

getFeed2DeepR needle = do
    (_, deps) <- getDeps True $ unpack needle
    feed2Helper needle deps

feed2Helper needle deps = do
    now <- liftIO getCurrentTime
    newsFeed Feed
        { feedTitle = pack $ "Newer dependencies for " ++ unpack needle
        , feedLinkSelf = Feed2R needle
        , feedLinkHome = RootR
        , feedUpdated = now
        , feedEntries = map go' deps
        , feedLanguage = "en"
        , feedDescription = toHtml $ "Newer dependencies for " ++ unpack needle
        }
  where
    go' ((name, version), (deps, time)) = FeedEntry
        { feedEntryLink = Feed3R needle (pack name) (pack $ display version) (pack $ show time)
        , feedEntryUpdated = time
        , feedEntryTitle = pack $ "Outdated dependencies for " ++ name ++ " " ++ display version
        , feedEntryContent = [shamlet|
<table border="1">
    $forall d <- deps
        <tr>
            <th>#{fst d}
            <td>#{snd d}
|]
        }

getFeed3R :: Text -> Text -> Text -> Text -> Handler ()
getFeed3R _ package _ _ =
    redirectText RedirectPermanent
  $ pack
  $ "http://hackage.haskell.org/package/" ++ unpack package

main = do
    args <- getArgs
    if args == ["--save-newest"]
        then do
            newest <- loadNewest
            writeFile "newest" $ show newest
        else do
            newest <- read `fmap` readFile "newest"
            warpDebug 5005 $ PD newest $ getReverses newest

getSpecificR :: Handler RepHtml
getSpecificR = do
    packages' <- lookupGetParams "package"
    PD newest _ <- getYesod
    let packages = map (id &&& flip getPackage newest) $ map unpack packages'
    let title = "Newer dependencies for your Hackage packages"
    let checkDeps' x =
            case checkDeps newest x of
                (_, _, AllNewest) -> Nothing
                (_, v, WontAccept cd _) -> Just (v, cd)
    defaultLayout $ do
        setTitle $ toHtml title
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
        let feedR = SpecificFeedR $ pack $ unwords $ map unpack packages'
        atomLink feedR title
        [whamlet|
<h1>#{title}
<p>
    The following are the packages which have restrictive upper bounds. You can also #
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
    let descs = mapMaybe (flip getPackage newest) $ words $ unpack packages'
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
        { feedEntryLink = Feed3R packages' (pack name) (pack $ display version) (pack $ show time)
        , feedEntryUpdated = time
        , feedEntryTitle = pack $ "Outdated dependencies for " ++ name ++ " " ++ display version
        , feedEntryContent = [shamlet|
<table border="1">
    $forall d <- deps
        <tr>
            <th>#{fst d}
            <td>#{snd d}
|]
        }

getReverseListR :: Handler RepHtml
getReverseListR = do
    PD _ reverse <- getYesod
    defaultLayout $ do
        setTitle "Reverse Dependencies"
        addCassius mainCassius
        addHamlet [$hamlet|
<h1>Reverse Dependencies
<p>Please choose a package below to view its reverse dependencies: those packages that depend upon it. This listing will also tell you which packages are incompatible with the current version of the package.
<table
    <tr
        <th>Package
        <th>Total Dependencies
        <th>Total Outdated Dependencies
    $forall p <- Map.toList reverse
        <tr>
            <td
                <a href=@{ReverseR $ pack $ fst p}>#{fst p}
            <td>#{show $ length $ snd $ snd p}
            $maybe o <- getOutdated $ snd p
                <td style="color:#900">#{o}
            $nothing
                <td>0
|]
  where
    getOutdated (version, pairs) =
        case filter (not . withinRange version . snd) pairs of
            [] -> Nothing
            ps -> Just $ show $ length ps

getReverseR :: Text -> Handler RepHtml
getReverseR dep = do
    PD _ reverse <- getYesod
    (version, rels) <- maybe notFound return $ Map.lookup (unpack dep) reverse
    defaultLayout $ do
        setTitle [shamlet|Reverse dependencies for #{dep}|]
        addCassius mainCassius
        addHamlet [hamlet|
<h1>#{show $ length rels} reverse dependencies for #{dep}&nbsp;#{display version}
<table>
    <tr>
        <th>Package
        <th>Uses current version?
    $forall rel <- rels
        <tr>
            $if Map.member (fst rel) reverse
                <td><a href=@{ReverseR $ pack $ fst rel}>#{fst rel}
            $else
                <td>#{fst rel}
            $if withinRange version (snd rel)
                <td>#{display (snd rel)}
            $else
                <td style="background-color:#fbb">#{display (snd rel)}
|]
