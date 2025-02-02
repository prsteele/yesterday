module Yesterday.Interpreter where

import Control.Monad
import Control.Monad.Reader
import Data.Foldable
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.FilePath
import Yesterday.App
import Yesterday.Parser (parseModule)
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

pprint :: Either History Value -> App T.Text
pprint (Right val) = pure (pprintValue val)
pprint (Left history) = pprintHistory history

pprintValue :: Value -> T.Text
pprintValue = T.pack . show

pprintHistory :: History -> App T.Text
pprintHistory (History _ _ _ focus) = pprintFocus focus

pprintFocus :: Maybe (Either History Value) -> App T.Text
pprintFocus Nothing = pure "?"
pprintFocus (Just (Right val)) = pure (pprintValue val)
pprintFocus (Just (Left history)) = pprintHistory history

pathToName :: FilePath -> T.Text
pathToName = T.pack . takeBaseName

topLevel :: Function -> [Expr] -> Expr
topLevel f = ECall (ELit (Func f))

eval :: Expr -> App (Either History Value)
eval (ELit (Var v)) = Left <$> evalVar v
eval (ELit value) = pure (Right value)
eval (EHist hexpr ops) = Left <$> evalHistOps hexpr ops
eval (EDeref expr) = evalDeref expr
eval (ECall fexpr argExprs) = Left <$> evalFunCall fexpr argExprs

evalVar :: Variable -> App History
evalVar v = do
  Frame _ historiesRef <- currentFrame
  histories <- liftIO (readIORef historiesRef)
  case M.lookup v histories of
    Just h -> pure h
    Nothing -> do
      h <- emptyHistory (Just v)
      let histories' = M.insert v h histories
      liftIO $ writeIORef historiesRef histories'
      pure h

emptyHistory :: Maybe Variable -> App History
emptyHistory mv = History mv Nothing <$> new [] <*> pure Nothing
  where
    new = liftIO . newIORef

evalHistOps :: Expr -> [HistOp] -> App History
evalHistOps expr ops = do
  hist <- evalToHist expr
  foldlM applyHistOp hist ops

applyHistOp :: History -> HistOp -> App History
applyHistOp hist (HistParent expr) = do
  n <- evalToInt expr
  when (n < 0) $ throwYesterday (YesterdayError ("~ must receive a non-negative argument, not " <> T.pack (show n)))
  walkParent hist n
applyHistOp hist (HistChild expr) = do
  n <- evalToInt expr
  when (n < 1) $ throwYesterday (YesterdayError ("^ must receive a positive argument, not " <> T.pack (show n)))
  walkChild hist n

evalToHist :: Expr -> App History
evalToHist expr = do
  mHist <- eval expr
  case mHist of
    Right e -> throwYesterday (YesterdayTypeError "history" (pprintValue e))
    Left h -> pure h

evalToInt :: Expr -> App Integer
evalToInt expr = do
  result <- eval expr
  mValue <- case result of
    Left eHist -> focusOfHistory eHist
    Right value -> pure (Just value)

  case mValue of
    Nothing -> throwYesterday (YesterdayTypeError "integer" "?")
    Just v -> case v of
      IntegerLit n -> pure n
      x -> throwYesterday (YesterdayTypeError "integer" (pprintValue x))

walkParent :: History -> Integer -> App History
walkParent hist 0 = pure hist
walkParent (History _ Nothing _ _) _ = paradox
walkParent (History _ (Just h) _ _) n = walkParent h (n - 1)

walkChild :: History -> Integer -> App History
walkChild (History _ _ childRef _) n = do
  children <- liftIO (readIORef childRef)

  let n' = fromIntegral n

  if n' <= length children
    then pure (children !! (n' - 1))
    else paradox

focusOfHistory :: History -> App (Maybe Value)
focusOfHistory (History _ _ _ Nothing) = pure Nothing
focusOfHistory (History _ _ _ (Just (Left history))) = focusOfHistory history
focusOfHistory (History _ _ _ (Just (Right value))) = pure (Just value)

evalDeref :: Expr -> App (Either History Value)
evalDeref expr = do
  History _ _ _ payload <- evalToHist expr
  maybe paradox pure payload

evalToFun :: Expr -> App Function
evalToFun = undefined

evalFunCall :: Expr -> [Expr] -> App History
evalFunCall fexpr argExprs = do
  fun <- evalToFun fexpr
  args <- mapM evalToHist argExprs
  frame <- pushFrame fun

  let hRefs = _histories frame
  forM_ args $ \hist@(History mvar _ _ _) -> do
    case mvar of
      Nothing -> pure ()
      Just var -> liftIO $ modifyIORef' hRefs (M.insert var hist)

  evalFunction frame fun args

evalFunction :: Frame -> Function -> [History] -> App History
evalFunction = undefined
