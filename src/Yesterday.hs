module Yesterday where

import Control.Monad
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Yesterday.App
import Yesterday.Interpreter
import Yesterday.Types

newtype YesterdayOpts = YesterdayOpts
  {_debug :: Bool}
  deriving (Show)

mainYesterday :: YesterdayOpts -> FilePath -> FilePath -> [String] -> IO ()
mainYesterday opts libDir mainFile args = do
  ref <- newIORef M.empty
  let env = YesterdayEnv libDir ref
  flip runYesterdayT env $ do
    mainF <- getFunction (pathToName mainFile)
    yargs <- parseCommandLineArgs args
    result <- eval (topLevel mainF yargs)
    when (_debug opts) $ pprint result

parseCommandLineArgs :: [String] -> App [Expr]
parseCommandLineArgs = mapM (parseYesterdayExpr . T.pack)
