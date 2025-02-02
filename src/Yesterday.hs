module Yesterday where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Yesterday.App
import Yesterday.Interpreter
import Yesterday.Types

newtype YesterdayOpts = YesterdayOpts
  {_debug :: Bool}
  deriving (Show)

mainYesterday :: YesterdayOpts -> FilePath -> FilePath -> [String] -> IO ()
mainYesterday opts libDir mainFile args = do
  env <- YesterdayEnv libDir <$> newIORef M.empty <*> newIORef []
  flip runYesterdayT env $ do
    mainF <- getFunction (pathToName mainFile)
    yargs <- parseCommandLineArgs args
    result <- eval (topLevel mainF yargs)
    when (_debug opts) $ pprint result >>= liftIO . TIO.putStrLn

parseCommandLineArgs :: [String] -> App [Expr]
parseCommandLineArgs = mapM (parseYesterdayExpr . T.pack)
