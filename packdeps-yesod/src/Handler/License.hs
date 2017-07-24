module Handler.License where

import Import
import Distribution.PackDeps.Types
import Text.Lucius (luciusFile)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.IORef (readIORef)
import qualified Data.Set as Set

getLicenses :: Handler (LicenseMap, [(Text, Text)])
getLicenses = do
    mdata <- (appData <$> getYesod) >>= liftIO . readIORef
    x <- lookupGetParam "include-tests"
    let includeTests = x == Just "true"
    maybe (error "Still loading data, please wait") (\(_, _, (notests, withtests), _) -> return $
        if includeTests
            then (withtests, [("include-tests", "true")])
            else (notests, [])) mdata

addToggle :: Widget
addToggle = do
    x <- lookupGetParam "include-tests"
    let includeTests = x == Just "true"
    if includeTests
        then [whamlet|<a href="?include-tests=false">Exclude test and benchmark dependencies|]
        else [whamlet|<a href="?include-tests=true">Include test and benchmark dependencies|]

getLicensesR :: Handler Html
getLicensesR = do
    (m, params) <- getLicenses
    defaultLayout $ do
        setTitle "Licenses"
        addToggle
        toWidget $(luciusFile "templates/home.lucius")
        $(widgetFile "licenses")

getLicenseR :: Text -> Handler Html
getLicenseR package = do
    (m, params) <- getLicenses
    Licenses licenses <- maybe notFound return $ Map.lookup (mkPackageName package) m
    defaultLayout $ do
        setTitle $ toHtml $ package <> " :: Licenses"
        addToggle
        toWidget $(luciusFile "templates/home.lucius")
        toWidget $(luciusFile "templates/licenses.lucius")
        $(widgetFile "license")
