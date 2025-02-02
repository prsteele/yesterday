module Yesterday.Interpreter where

import Control.Monad
import Control.Monad.Reader
import Data.Foldable
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath
import System.IO
import UnliftIO
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

pprint :: Either Focus Value -> App T.Text
pprint (Right val) = pure (pprintValue val)
pprint (Left focus) = pprintFocus focus

pprintValue :: Value -> T.Text
pprintValue = T.pack . show

pprintFocus :: Focus -> App T.Text
pprintFocus (Focus _ (History _ _ payload)) = pprintPayload payload

pprintPayload :: Maybe (Either Focus Value) -> App T.Text
pprintPayload Nothing = pure "?"
pprintPayload (Just (Right val)) = pure (pprintValue val)
pprintPayload (Just (Left focus)) = do
  inner <- pprintFocus focus
  pure ("history -> " <> inner)

pathToName :: FilePath -> T.Text
pathToName = T.pack . takeBaseName

topLevel :: Function -> [Expr] -> Expr
topLevel f = ECall (ELit (Func f))

eval :: Expr -> App (Either Focus Value)
eval (ELit (Var v)) = Left <$> evalVar v
eval (ELit value) = pure (Right value)
eval (EHist focusExpr ops) = Left <$> evalHistOps focusExpr ops
eval (EDeref expr) = evalDeref expr
eval (EAdd lexpr rexpr) = Right <$> evalBinOp (+) lexpr rexpr
eval (EMul lexpr rexpr) = Right <$> evalBinOp (*) lexpr rexpr
eval (ESub lexpr rexpr) = Right <$> evalBinOp (-) lexpr rexpr
eval (EDiv lexpr rexpr) = Right <$> evalBinOp div lexpr rexpr
eval (ECall fexpr argExprs) = Left <$> evalFunCall fexpr argExprs

evalVar :: Variable -> App Focus
evalVar v = do
  Frame _ fociRef <- currentFrame
  foci <- liftIO (readIORef fociRef)
  case M.lookup v foci of
    Just focus -> pure focus
    Nothing -> do
      focus <- emptyFocus (Just v)
      let foci' = M.insert v focus foci
      liftIO $ writeIORef fociRef foci'
      pure focus

emptyFocus :: Maybe Variable -> App Focus
emptyFocus mv =
  Focus mv
    <$> (History Nothing <$> liftIO (newIORef []) <*> pure Nothing)

evalHistOps :: Expr -> [HistOp] -> App Focus
evalHistOps expr ops = do
  hist <- evalToFocus expr
  foldlM applyHistOp hist ops

applyHistOp :: Focus -> HistOp -> App Focus
applyHistOp hist (HistParent expr) = do
  n <- evalToInt expr
  when (n < 0) $ throwYesterday (YesterdayError ("~ must receive a non-negative argument, not " <> T.pack (show n)))
  walkParent hist n
applyHistOp hist (HistChild expr) = do
  n <- evalToInt expr
  when (n < 1) $ throwYesterday (YesterdayError ("^ must receive a positive argument, not " <> T.pack (show n)))
  walkChild hist n

evalToFocus :: Expr -> App Focus
evalToFocus expr = do
  mHist <- eval expr
  case mHist of
    Right e -> throwYesterday (YesterdayTypeError "history" (pprintValue e))
    Left h -> pure h

evalToInt :: Expr -> App Integer
evalToInt expr = do
  result <- eval expr

  value <- case result of
    Left (Focus _ (History _ _ Nothing)) -> throwYesterday (YesterdayTypeError "integer" "?")
    Left (Focus _ (History _ _ (Just (Left _)))) -> paradox
    Left (Focus _ (History _ _ (Just (Right value)))) -> pure value
    Right value -> pure value

  case value of
    IntegerLit n -> pure n
    x -> throwYesterday (YesterdayTypeError "integer" (pprintValue x))

walkParent :: Focus -> Integer -> App Focus
walkParent focus 0 = pure focus
walkParent (Focus _ (History Nothing _ _)) _ = throwYesterday YesterdayIllegalRefocus
walkParent (Focus mv (History (Just (h, _)) _ _)) n = walkParent (Focus mv h) (n - 1)

walkChild :: Focus -> Integer -> App Focus
walkChild (Focus mv (History _ childRef _)) n = do
  children <- liftIO (readIORef childRef)

  let n' = fromIntegral n

  if n' <= length children
    then pure (Focus mv (children !! (n' - 1)))
    else throwYesterday YesterdayIllegalRefocus

evalDeref :: Expr -> App (Either Focus Value)
evalDeref expr = do
  (Focus _ (History _ _ payload)) <- evalToFocus expr
  maybe paradox pure payload

evalBinOp :: (Integer -> Integer -> Integer) -> Expr -> Expr -> App Value
evalBinOp op lhs rhs = IntegerLit <$> (op <$> evalToInt lhs <*> evalToInt rhs)

getRoot :: Focus -> Focus
getRoot (Focus mv h) = Focus mv h'
  where
    (h', _) = getRoot' h

getRoot' :: History -> (History, [Int])
getRoot' (History (Just (h, n)) _ _) = fmap (n :) (getRoot' h)
getRoot' h@(History Nothing _ _) = (h, [])

copyHist :: Focus -> Maybe Variable -> App Focus
copyHist (Focus _ h) mv =
  let (root, revPath) = getRoot' h

      unwind h' [] = pure h'
      unwind (History _ childrenRef _) (n : ns) = do
        children <- readIORef childrenRef
        unwind (children !! n) ns
   in do
        copied <- copyHist' root
        let (copied', _) = getRoot' copied
        Focus mv <$> unwind copied' (reverse revPath)

copyHist' :: History -> App History
copyHist' (History parent childrenRef payload) = History parent <$> childrenRef' <*> pure payload
  where
    childrenRef' = readIORef childrenRef >>= mapM copyHist' >>= newIORef

-- evalGets :: Expr -> Expr -> App History
-- evalGets lexpr rexpr = do
--   lhist <- evalToFocus lexpr
--   rhist <- evalToFocus rexpr

evalToFun :: Expr -> App Function
evalToFun = undefined

evalToBool :: Expr -> App Bool
evalToBool expr =
  let handler :: YesterdayIllegalRefocus -> App (Either Focus Value)
      handler _ = pure (Right (BoolLit False))
   in do
        result <- handle handler (eval expr) -- Illegal refocuses are falsy
        let value = case result of
              Left _ -> BoolLit True
              Right x -> x
        case value of
          BoolLit n -> pure n
          x -> throwYesterday (YesterdayTypeError "bool" (pprintValue x))

evalFunCall :: Expr -> [Expr] -> App Focus
evalFunCall fexpr argExprs = do
  fun <- evalToFun fexpr
  args <- mapM evalToFocus argExprs
  frame <- pushFrame fun

  let fociRefs = _foci frame
  forM_ args $ \focus@(Focus mvar _) -> do
    case mvar of
      Nothing -> pure ()
      Just var -> liftIO $ modifyIORef' fociRefs (M.insert var focus)

  result <- evalFunction frame fun
  popFrame
  pure result

evalFunction :: Frame -> Function -> App Focus
evalFunction frame func@(Function _ resultVar clauses) =
  let f [] = pure Nothing
      f (c@(Clause check _) : cs) = do
        match <- evalToBool check
        if match
          then pure (Just c)
          else f cs

      finishFunc = do
        foci <- readIORef (_foci frame)
        case M.lookup resultVar foci of
          Nothing -> emptyFocus (Just resultVar)
          Just result -> pure result

      applyAction (WriteStdout expr) = do
        x <- eval expr
        pprint x >>= liftIO . TIO.hPutStrLn stdout
      applyAction (WriteStderr expr) = do
        x <- eval expr
        pprint x >>= liftIO . TIO.hPutStrLn stderr
      applyAction (Gets lhs rhs) = void (applyGets lhs rhs)
      applyAction (PlusGets lhs rhs) = undefined -- void (applyPlusGets lhs rhs)
   in do
        mayMatchingClause <- f clauses

        case mayMatchingClause of
          Nothing -> finishFunc
          Just (Clause _ actions) -> mapM_ applyAction actions >> evalFunction frame func

applyGets :: Expr -> Expr -> App Focus
applyGets lhs rhs = do
  Focus mv (History _ childrenRef _) <- evalToFocus lhs
  branch <- evalToFocus rhs >>= (`copyHist` mv)
  let (Focus _ (History _ branchRootChildrenRef _)) = getRoot branch
  branchRootChildren <- readIORef branchRootChildrenRef
  atomicModifyIORef' childrenRef (\children -> (children <> branchRootChildren, ()))
  pure branch

-- applyPlusGets :: Expr -> Expr -> App Focus
-- applyPlusGets lhs rhs = do
--   Focus mv (History _ childrenRef _) <- evalToFocus lhs
