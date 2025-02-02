module Yesterday.App where

import Control.Monad.Reader
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Typeable
import UnliftIO
import Yesterday.Parser
import Yesterday.Types

data YesterdayEnv = YesterdayEnv
  { _yesterdayLibDir :: FilePath,
    _yesterdayModules :: IORef (M.Map T.Text Function),
    _yesterdayStack :: IORef [Frame]
  }

currentFrame :: App Frame
currentFrame = do
  frames <- asks _yesterdayStack >>= liftIO . readIORef
  case frames of
    [] -> throwYesterday YesterdayEmptyStack
    (f : _) -> pure f

pushFrame :: Function -> App Frame
pushFrame func = do
  frame <- Frame func <$> liftIO (newIORef M.empty)
  framesRef <- asks _yesterdayStack
  liftIO $ atomicModifyIORef' framesRef (\frames -> (frame : frames, ()))
  pure frame

popFrame :: App ()
popFrame = do
  framesRef <- asks _yesterdayStack
  frames <- liftIO (readIORef framesRef)

  case frames of
    [] -> throwYesterday YesterdayEmptyStack
    (_ : xs) -> liftIO (writeIORef framesRef xs)

newtype YesterdayT m a = YesterdayT {unYesterdayT :: ReaderT YesterdayEnv m a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader YesterdayEnv,
      MonadIO,
      MonadUnliftIO
    )

type App = YesterdayT IO

throwYesterday :: (Exception e) => e -> App a
throwYesterday = liftIO . throwIO

paradox :: App a
paradox = throwYesterday YesterdayParadox

newtype YesterdayError = YesterdayError T.Text
  deriving (Show, Typeable)

data YesterdayParadox = YesterdayParadox
  deriving (Show, Typeable)

data YesterdayTypeError = YesterdayTypeError T.Text T.Text
  deriving (Show, Typeable)

newtype YesterdayNoSuchFunction = YesterdayNoSuchFunction T.Text
  deriving (Show, Typeable)

newtype YesterdayNoSuchFile = YesterdayNoSuchFile FilePath
  deriving (Show, Typeable)

data YesterdayParseError = YesterdayParseError FilePath T.Text
  deriving (Show, Typeable)

data YesterdayEmptyStack = YesterdayEmptyStack
  deriving (Show, Typeable)

data YesterdayIllegalRefocus = YesterdayIllegalRefocus
  deriving (Show, Typeable)

instance Exception YesterdayError

instance Exception YesterdayNoSuchFile

instance Exception YesterdayNoSuchFunction

instance Exception YesterdayParseError

instance Exception YesterdayEmptyStack

instance Exception YesterdayTypeError

instance Exception YesterdayParadox

instance Exception YesterdayIllegalRefocus

runYesterdayT :: YesterdayT m a -> YesterdayEnv -> m a
runYesterdayT = runReaderT . unYesterdayT

parseYesterdayFile :: FilePath -> App Function
parseYesterdayFile fname =
  let handleMissing :: IOException -> IO a
      handleMissing _ = throwIO (YesterdayNoSuchFile fname)
   in liftIO $ do
        contents <- handle handleMissing (TIO.readFile fname)
        case parse fname yesterdayFunction contents of
          Left err -> throwIO (YesterdayParseError fname err)
          Right f -> pure f

parseYesterdayExpr :: T.Text -> App Expr
parseYesterdayExpr x =
  case parse "<command line>" expr x of
    Left err -> throwYesterday (YesterdayParseError "<command line>" err)
    Right result -> pure result
