import Prelude              (IO)
import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Settings             (parseExtra)
import Application          (makeApplication)
import System.IO            (hSetBuffering, stdout, stderr, BufferMode (LineBuffering))

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  defaultMain (fromArgs parseExtra) makeApplication
