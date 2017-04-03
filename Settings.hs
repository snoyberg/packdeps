-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings
    ( widgetFile
    , staticDir
    , Extra (..)
    , parseExtra
    ) where

import Prelude
import Text.Shakespeare.Text (st)
import Language.Haskell.TH.Syntax
import Yesod.Default.Config
import qualified Yesod.Default.Util
import Yesod.Default.Util
import Data.Default (def)
import Text.Hamlet
import Data.Text (Text)
import Data.Yaml
import Control.Applicative
import Settings.Development

-- Static setting below. Changing these requires a recompile

-- | The location of static files on your system. This is a file system
-- path. The default value works properly with your scaffolded site.
staticDir :: FilePath
staticDir = "static"


-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def
    { wfsHamletSettings = defaultHamletSettings
        { hamletNewlines = AlwaysNewlines
        }
    }
widgetFile :: String -> Q Exp
widgetFile = (if development then widgetFileReload
                             else widgetFileNoReload)
              widgetFileSettings

data Extra = Extra
    { extraCopyright :: Text
    , extraAnalytics :: Maybe Text -- ^ Google Analytics
    } deriving Show

parseExtra :: DefaultEnv -> Object -> Parser Extra
parseExtra _ o = Extra
    <$> o .:  "copyright"
    <*> o .:? "analytics"
