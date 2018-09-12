module Handler.Home where

import Import
import Yesod.Feed
import Yesod.AtomFeed
import Text.Lucius (luciusFile)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Distribution.PackDeps
    ( CheckDepsRes (AllNewest, WontAccept)
    , checkDeps, getPackage
    , Outdated
    )
import Data.Monoid ((<>))
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Time (getCurrentTime, UTCTime)
import Distribution.PackDeps.Util (withinRange)
import Control.Arrow ((&&&))
import Data.List (sortBy)
import Data.Ord (comparing)
import Distribution.PackDeps.Types (Version, PackageName, unPackageName, mkPackageName, VersionRange)
import Yesod.Form.Jquery (urlJqueryJs)

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "Hackage dependency monitor"
    $(widgetFile "home")

getFeedR :: Handler Html
getFeedR = do
    needle <- runInputGet $ ireq textField "needle"
    deep <- isDeep
    (descs, deps) <- getDeps deep needle
    let title = "Newer dependencies for " <> needle
    let deepR = (FeedR, [("needle", needle), ("deep", "on")])
    defaultLayout $ do
        setTitle $ toHtml title
        let feedR = (if deep then Feed2DeepR else Feed2R) needle
        $(widgetFile "feed")
        atomLink feedR title
  where
    fst3 (x, _, _) = x

getFeed2R :: Text -> Handler TypedContent
getFeed2R needle = do
    (_, deps) <- getDeps False needle
    feed2Helper needle deps

getFeed2DeepR :: Text -> Handler TypedContent
getFeed2DeepR needle = do
    (_, deps) <- getDeps True needle
    feed2Helper needle deps

feed2Helper :: Text
            -> [((Text, Version), (HashMap PackageName Outdated, UTCTime))]
            -> Handler TypedContent
feed2Helper needle deps = do
    now <- liftIO getCurrentTime
    newsFeed Feed
        { feedTitle = "Newer dependencies for " <> needle
        , feedAuthor = "PackDeps"
        , feedLinkSelf = Feed2R needle
        , feedLinkHome = HomeR
        , feedUpdated = now
        , feedEntries = map go' deps
        , feedLanguage = "en"
        , feedDescription = toHtml $ "Newer dependencies for " <> needle
        , feedLogo = Nothing
        }
  where
    go' ((name, version), (deps', time)) = FeedEntry
        { feedEntryLink = Feed3R needle name (pack $ show version) (pack $ show time)
        , feedEntryUpdated = time
        , feedEntryTitle = "Outdated dependencies for " <> name <> " " <> pack (show version)
        , feedEntryContent = [shamlet|
<table border=1>
    $forall d <- H.toList deps'
        <tr>
            <th>#{unPackageName $ fst d}
            <td>#{show $ snd d}
|]
        , feedEntryEnclosure = Nothing
        }

getFeed3R :: Text -> Text -> Text -> Text -> Handler ()
getFeed3R _ package _ _ =
    redirect $ "http://hackage.haskell.org/package/" <> package

getSpecificR :: Handler Html
getSpecificR = do
    packages' <- lookupGetParams "package"
    (newest, _) <- getData
    let packages = map (id &&& flip getPackage newest) $ map mkPackageName packages'
    let title = "Newer dependencies for your Hackage packages" :: Text
    let checkDeps' x =
            case checkDeps newest x of
                (_, _, AllNewest) -> Nothing
                (_, v, WontAccept cd _) -> Just (v, cd)
    defaultLayout $ do
        setTitle $ toHtml title
        let feedR = SpecificFeedR $ T.unwords packages'
        $(widgetFile "specific")
        atomLink feedR title

getSpecificFeedR :: Text -> Handler TypedContent
getSpecificFeedR packages' = do
    (newest, _) <- getData
    let descs = mapMaybe (flip getPackage newest . mkPackageName) $ T.words packages'
    let go (_, _, AllNewest) = Nothing
        go (unPackageName -> x, v, WontAccept y z) = Just ((x, v), (y, z))
        deps = reverse $ sortBy (comparing $ snd . snd)
             $ mapMaybe (go . checkDeps newest) descs
    now <- liftIO getCurrentTime
    newsFeed Feed
        { feedTitle = "Newer dependencies for Hackage packages"
        , feedAuthor = "PackDeps"
        , feedLinkSelf = SpecificFeedR packages'
        , feedLinkHome = HomeR
        , feedUpdated = now
        , feedEntries = map go' deps
        , feedLanguage = "en"
        , feedDescription = "Newer dependencies for Hackage packages"
        , feedLogo = Nothing
        }
  where
    go' ((name, version), (deps, time)) = FeedEntry
        { feedEntryLink = Feed3R packages' name (pack $ show version) (pack $ show time)
        , feedEntryUpdated = time
        , feedEntryTitle = "Outdated dependencies for " <> name <> " " <> pack (show version)
        , feedEntryContent = [shamlet|
<table border=1>
    $forall d <- H.toList deps
        <tr>
            <th>#{unPackageName $ fst d}
            <td>#{show $ snd d}
|]
        , feedEntryEnclosure = Nothing
        }

getReverseListR :: Handler Html
getReverseListR = do
    (_, reverse') <- getData
    defaultLayout $ do
        setTitle "Reverse Dependencies"
        toWidget $(luciusFile "templates/home.lucius")
        $(widgetFile "reverselist")
  where
    getOutdated :: ((Version, b), HashMap a (VersionRange Version, Text)) -> Maybe String
    getOutdated ((version, _), pairs) =
        case filter (not . withinRange version . fst . snd) $ H.toList pairs of
            [] -> Nothing
            ps -> Just $ show $ length ps

getReverseR :: Text -> Handler Html
getReverseR dep = do
    (_, reverse') <- getData
    ((version, syn), rels) <- maybe notFound return $ H.lookup (mkPackageName dep) reverse'
    y <- getYesod
    defaultLayout $ do
        setTitle [shamlet|Reverse dependencies for #{dep}|]
        toWidget $(luciusFile "templates/home.lucius")
        addScriptEither $ urlJqueryJs y
        $(widgetFile "reverse")
  where
    plural :: Int -> String -> String -> String
    plural 1 s _ = s
    plural _ _ pl = pl

sortByName :: [(PackageName, a)] -> [(PackageName, a)]
sortByName =
    map snd . sortBy (comparing fst) . map (\(x, y) -> (T.toCaseFold $ unPackageName x, (x, y)))

sortCI :: [PackageName] -> [PackageName]
sortCI = map snd . sortBy (comparing fst) . map (\t -> (T.toCaseFold $ unPackageName t, t))
