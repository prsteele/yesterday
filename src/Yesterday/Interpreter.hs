module Yesterday.Interpreter where

import Control.Monad.Reader
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.FilePath
import Yesterday.App
import Yesterday.Parser
import Yesterday.Types

-- | Load a function, importing if necessary.
getFunction :: T.Text -> App Function
getFunction name = do
  -- See if the function already exists
  modules <- asks _yesterdayModules >>= liftIO . readIORef
  case M.lookup name modules of
    Just f -> pure f
    Nothing -> do
      libDir <- asks _yesterdayLibDir
      importFunction (libDir </> T.unpack name)

-- | Import a function from disk into the Yesterday environment.
importFunction :: FilePath -> App Function
importFunction path = do
  eModule <- liftIO $ parseModule path
  case eModule of
    Left err -> throwYesterday (YesterdayNoSuchFunction err)
    Right f -> do
      mref <- asks _yesterdayModules
      modules <- liftIO (readIORef mref)
      let modules' = M.insert (pathToName path) f modules
      liftIO (writeIORef mref modules')
      pure f

pprint :: Either History Value -> App ()
pprint (Right val) = pprintValue val
pprint (Left history) = pprintHistory history

pprintValue :: Value -> App ()
pprintValue = liftIO . print

pprintHistory :: History -> App ()
pprintHistory (History _ _ focus) = pprintFocus focus

pprintFocus :: Maybe (Either History Value) -> App ()
pprintFocus Nothing = liftIO (putStrLn "?")
pprintFocus (Just (Right val)) = pprintValue val
pprintFocus (Just (Left history)) = pprintHistory history

pathToName :: FilePath -> T.Text
pathToName = T.pack . takeBaseName

topLevel :: Function -> [Expr] -> Expr
topLevel f = ECall (ELit (Func f))

eval :: Expr -> App (Either History Value)
eval = undefined
