module Yesterday.App where

import Control.Exception
import Control.Monad.Reader
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Typeable
import Yesterday.Parser
import Yesterday.Types

data YesterdayEnv = YesterdayEnv
  { _yesterdayLibDir :: FilePath,
    _yesterdayModules :: IORef (M.Map T.Text Function)
  }

newtype YesterdayT m a = YesterdayT {unYesterdayT :: ReaderT YesterdayEnv m a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader YesterdayEnv,
      MonadIO
    )

type App = YesterdayT IO

throwYesterday :: (Exception e) => e -> App a
throwYesterday = liftIO . throwIO

newtype YesterdayError = YesterdayError T.Text
  deriving (Show, Typeable)

newtype YesterdayNoSuchFunction = YesterdayNoSuchFunction T.Text
  deriving (Show, Typeable)

newtype YesterdayNoSuchFile = YesterdayNoSuchFile FilePath
  deriving (Show, Typeable)

newtype YesterdayParseError = YesterdayParseError (FilePath, T.Text)
  deriving (Show, Typeable)

instance Exception YesterdayError

instance Exception YesterdayNoSuchFile

instance Exception YesterdayNoSuchFunction

instance Exception YesterdayParseError

runYesterdayT :: YesterdayT m a -> YesterdayEnv -> m a
runYesterdayT = runReaderT . unYesterdayT

parseYesterdayFile :: FilePath -> App Function
parseYesterdayFile fname =
  let handleMissing :: IOException -> IO a
      handleMissing _ = throwIO (YesterdayNoSuchFile fname)
   in liftIO $ do
        contents <- handle handleMissing (TIO.readFile fname)
        case parse fname yesterdayFunction contents of
          Left err -> throwIO (YesterdayParseError (fname, err))
          Right f -> pure f

parseYesterdayExpr :: T.Text -> App Expr
parseYesterdayExpr x =
  case parse "<command line>" expr x of
    Left err -> throwYesterday (YesterdayParseError ("<command line>", err))
    Right result -> pure result
