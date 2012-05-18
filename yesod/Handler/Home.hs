module Handler.Home where

import Import
import Distribution.Package
import Yesod.Feed
import Yesod.AtomFeed
import Text.Hamlet (shamlet)
import Text.Lucius (luciusFile)
import qualified Data.Map as Map
import Distribution.PackDeps
    ( CheckDepsRes (AllNewest, WontAccept)
    , checkDeps, getPackage, diName
    )
import Data.Maybe (mapMaybe)
import Data.Text (unpack)
import Data.Time (getCurrentTime, UTCTime)
import Distribution.Version (withinRange)
import Control.Arrow ((&&&))
import Data.List (sortBy, sort)
import Data.Ord (comparing)
import Data.Version (Version)
import Distribution.Text (display)

getHomeR :: Handler RepHtml
getHomeR = defaultLayout $ do
    setTitle "Hackage dependency monitor"
    $(widgetFile "home")

getFeedR :: Handler RepHtml
getFeedR = do
    needle <- runInputGet $ ireq textField "needle"
    deep <- isDeep
    (descs, deps) <- getDeps deep $ unpack needle
    let title = "Newer dependencies for " <> needle
    let deepR = (FeedR, [("needle", needle), ("deep", "on")])
    defaultLayout $ do
        setTitle $ toHtml title
        let feedR = (if deep then Feed2DeepR else Feed2R) needle
        $(widgetFile "feed")
        atomLink feedR title

getFeed2R :: Text -> Handler RepAtomRss
getFeed2R needle = do
    (_, deps) <- getDeps False $ unpack needle
    feed2Helper needle deps

getFeed2DeepR :: Text -> Handler RepAtomRss
getFeed2DeepR needle = do
    (_, deps) <- getDeps True $ unpack needle
    feed2Helper needle deps

feed2Helper :: Text
            -> [((String, Version), ([(String, String)], UTCTime))]
            -> Handler RepAtomRss
feed2Helper needle deps = do
    now <- liftIO getCurrentTime
    newsFeed Feed
        { feedTitle = "Newer dependencies for " <> needle
        , feedLinkSelf = Feed2R needle
        , feedLinkHome = HomeR
        , feedUpdated = now
        , feedEntries = map go' deps
        , feedLanguage = "en"
        , feedDescription = toHtml $ "Newer dependencies for " <> needle
        }
  where
    go' ((name, version), (deps', time)) = FeedEntry
        { feedEntryLink = Feed3R needle (pack name) (pack $ display version) (pack $ show time)
        , feedEntryUpdated = time
        , feedEntryTitle = pack $ "Outdated dependencies for " <> name <> " " <> display version
        , feedEntryContent = [shamlet|
<table border=1>
    $forall d <- deps'
        <tr>
            <th>#{fst d}
            <td>#{snd d}
|]
        }

getFeed3R :: Text -> Text -> Text -> Text -> Handler ()
getFeed3R _ package _ _ =
    redirect $ "http://hackage.haskell.org/package/" <> package

getSpecificR :: Handler RepHtml
getSpecificR = do
    packages' <- lookupGetParams "package"
    (newest, _) <- getData
    let packages = map (id &&& flip getPackage newest) $ map unpack packages'
    let title = "Newer dependencies for your Hackage packages"
    let checkDeps' x =
            case checkDeps newest x of
                (_, _, AllNewest) -> Nothing
                (_, v, WontAccept cd _) -> Just (v, cd)
    defaultLayout $ do
        setTitle $ toHtml title
        let feedR = SpecificFeedR $ pack $ unwords $ map unpack packages'
        $(widgetFile "specific")
        atomLink feedR title

getSpecificFeedR :: Text -> Handler RepAtomRss
getSpecificFeedR packages' = do
    (newest, _) <- getData
    let descs = mapMaybe (flip getPackage newest) $ words $ unpack packages'
    let go (_, _, AllNewest) = Nothing
        go (PackageName x, v, WontAccept y z) = Just ((x, v), (y, z))
        deps = reverse $ sortBy (comparing $ snd . snd)
             $ mapMaybe (go . checkDeps newest) descs
    now <- liftIO getCurrentTime
    newsFeed Feed
        { feedTitle = "Newer dependencies for Hackage packages"
        , feedLinkSelf = SpecificFeedR packages'
        , feedLinkHome = HomeR
        , feedUpdated = now
        , feedEntries = map go' deps
        , feedLanguage = "en"
        , feedDescription = "Newer dependencies for Hackage packages"
        }
  where
    go' ((name, version), (deps, time)) = FeedEntry
        { feedEntryLink = Feed3R packages' (pack name) (pack $ display version) (pack $ show time)
        , feedEntryUpdated = time
        , feedEntryTitle = pack $ "Outdated dependencies for " <> name <> " " <> display version
        , feedEntryContent = [shamlet|
<table border=1>
    $forall d <- deps
        <tr>
            <th>#{fst d}
            <td>#{snd d}
|]
        }

getReverseListR :: Handler RepHtml
getReverseListR = do
    (_, reverse') <- getData
    defaultLayout $ do
        setTitle "Reverse Dependencies"
        toWidget $(luciusFile "templates/home.lucius")
        $(widgetFile "reverselist")
  where
    getOutdated (version, pairs) =
        case filter (not . withinRange version . snd) pairs of
            [] -> Nothing
            ps -> Just $ show $ length ps

getReverseR :: Text -> Handler RepHtml
getReverseR dep = do
    (_, reverse') <- getData
    (version, rels) <- maybe notFound return $ Map.lookup (unpack dep) reverse'
    defaultLayout $ do
        setTitle [shamlet|Reverse dependencies for #{dep}|]
        toWidget $(luciusFile "templates/home.lucius")
        $(widgetFile "reverse")
  where
    plural :: Int -> String -> String -> String
    plural 1 s _ = s
    plural _ _ pl = pl
