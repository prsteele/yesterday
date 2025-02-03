module Yesterday.Interpreter where

import Control.Monad
import Control.Monad.Reader
import Data.Foldable
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit
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
eval (EEq lhs rhs) = Right . BoolLit <$> evalEQ lhs rhs
eval (ENEq lhs rhs) = Right . BoolLit <$> evalNotEQ lhs rhs
eval (ELT lhs rhs) = Right . BoolLit <$> evalLT lhs rhs
eval (ELE lhs rhs) = Right . BoolLit <$> evalLE lhs rhs
eval (EGT lhs rhs) = Right . BoolLit <$> evalGT lhs rhs
eval (EGE lhs rhs) = Right . BoolLit <$> evalGE lhs rhs
eval (ECall fexpr argExprs) = Left <$> evalFunCall fexpr argExprs
eval (EAction action expr) = Right SideEffect <$ applyAction action expr
eval (EGets lhs rhs) = Left <$> applyGets lhs rhs
eval (EPlusGets lhs rhs) = Left <$> applyPlusGets lhs rhs
eval (EAnd lhs rhs) = Right . BoolLit <$> evalAnd lhs rhs
eval (EOr lhs rhs) = Right . BoolLit <$> evalOr lhs rhs
eval (ENull x) = Right . BoolLit <$> evalNull x
eval (EInterrobang x) = Right . BoolLit <$> evalInterrobang x
eval (EHistLit x) = Left <$> evalHistLit x

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

evalToValueWithOneDereference :: Expr -> App Value
evalToValueWithOneDereference expr = do
  result <- eval expr
  case result of
    Right x -> pure x
    Left (Focus _ (History _ _ (Just (Right x)))) -> pure x
    Left (Focus _ _) -> paradox

evalToBoolWithOneDereference :: Expr -> App Bool
evalToBoolWithOneDereference expr = do
  result <- evalToValueWithOneDereference expr
  case result of
    BoolLit x -> pure x
    _ -> paradox

evalLT :: Expr -> Expr -> App Bool
evalLT lhs rhs = do
  l <- evalToValueWithOneDereference lhs
  r <- evalToValueWithOneDereference rhs

  case (l, r) of
    (BoolLit x, BoolLit y) -> pure (x < y)
    (IntegerLit x, IntegerLit y) -> pure (x < y)
    (StringLit x, StringLit y) -> pure (x < y)
    _ -> paradox

evalLE :: Expr -> Expr -> App Bool
evalLE lhs rhs = not <$> evalGT lhs rhs

evalGT :: Expr -> Expr -> App Bool
evalGT lhs rhs = evalLT rhs lhs

evalGE :: Expr -> Expr -> App Bool
evalGE lhs rhs = not <$> evalLT lhs rhs

evalEQ :: Expr -> Expr -> App Bool
evalEQ lhs rhs = (&&) <$> evalLE lhs rhs <*> evalGE lhs rhs

evalAnd :: Expr -> Expr -> App Bool
evalAnd lhs rhs = (&&) <$> evalToBool lhs <*> evalToBool rhs

evalOr :: Expr -> Expr -> App Bool
evalOr lhs rhs = (||) <$> evalToBool lhs <*> evalToBool rhs

evalNotEQ :: Expr -> Expr -> App Bool
evalNotEQ lhs rhs = not <$> evalEQ lhs rhs

-- eval (EInterrobang x) = Right . BoolLit <$> evalInterrobang x
-- eval (EHistLit x) = Left <$> evalHistLit x

evalNull :: Variable -> App Bool
evalNull var = do
  Frame _ fociRef <- currentFrame
  foci <- liftIO $ readIORef fociRef
  pure (M.member var foci)

evalInterrobang :: Expr -> App Bool
evalInterrobang expr = do
  result <- eval expr
  case result of
    Left (Focus _ (History _ _ Nothing)) -> pure True
    _ -> pure False

evalHistLit :: Expr -> App Focus
evalHistLit expr = do
  root <- emptyFocus Nothing
  applyPlusGets' root expr

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

getRoot :: Focus -> App Focus
getRoot (Focus mv h) = Focus mv . fst <$> getRoot' h

getRoot' :: History -> App (History, [Int])
getRoot' (History (Just (h, nRef)) _ _) = do
  (h', path) <- getRoot' h
  n <- readIORef nRef
  pure (h', n : path)
getRoot' h@(History Nothing _ _) = pure (h, [])

copyHist :: Focus -> Maybe Variable -> App Focus
copyHist (Focus _ h) mv =
  let unwind h' [] = pure h'
      unwind (History _ childrenRef _) (n : ns) = do
        children <- readIORef childrenRef
        unwind (children !! n) ns
   in do
        (root, revPath) <- getRoot' h
        copied <- copyHist' root
        (copied', _) <- getRoot' copied
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

  result <- evalFunction frame
  popFrame
  pure result

evalFunction :: Frame -> App Focus
evalFunction frame@(Frame (Function _ resultVar clauses) focusRefs) =
  let f [] = pure Nothing
      f (c@(Clause check _) : cs) = do
        match <- evalToBool check
        if match
          then pure (Just c)
          else f cs

      finishFunc = do
        foci <- readIORef focusRefs
        case M.lookup resultVar foci of
          Nothing -> emptyFocus (Just resultVar)
          Just result -> pure result
   in do
        mayMatchingClause <- f clauses

        mFocus <- case mayMatchingClause of
          Nothing -> fmap Just finishFunc
          Just (Clause _ exprs) -> do
            forM_ exprs $ \expr -> do
              result <- eval expr
              case result of
                Left focus@(Focus (Just var) _) -> modify' focusRefs (M.insert var focus)
                _ -> pure ()
            pure Nothing

        maybe (evalFunction frame) pure mFocus

applyAction :: Action -> Expr -> App ()
applyAction WriteStdout expr = do
  x <- eval expr
  pprint x >>= liftIO . TIO.hPutStrLn stdout
applyAction WriteStderr expr = do
  x <- eval expr
  pprint x >>= liftIO . TIO.hPutStrLn stderr

evalGets :: Expr -> Expr -> App Focus
evalGets = undefined

evalPlusGets :: Expr -> Expr -> App Focus
evalPlusGets = undefined

applyGets :: Expr -> Expr -> App Focus
applyGets lhs rhs =
  let adjustChildren offset children = forM_ children $ \(History parent _ _) -> do
        case parent of
          Nothing -> pure () -- Impossible
          Just (_, cRef) -> modify' cRef (+ offset)
   in do
        Focus mv (History _ childrenRef _) <- evalToFocus lhs
        lhsChildren <- readIORef childrenRef
        let offset = length lhsChildren

        branch <- evalToFocus rhs >>= (`copyHist` mv)
        Focus _ (History _ branchRootChildrenRef _) <- getRoot branch
        branchRootChildren <- readIORef branchRootChildrenRef
        adjustChildren offset branchRootChildren

        modify' childrenRef (<> branchRootChildren)
        pure branch

applyPlusGets :: Expr -> Expr -> App Focus
applyPlusGets lhs rhs = do
  focus <- evalToFocus lhs
  applyPlusGets' focus rhs

applyPlusGets' :: Focus -> Expr -> App Focus
applyPlusGets' (Focus mv parent@(History _ childrenRef _)) rhs = do
  childCount <- length <$> readIORef childrenRef
  childNumRef <- newIORef childCount
  child <- History (Just (parent, childNumRef)) <$> newIORef [] <*> (Just <$> eval rhs)
  modify' childrenRef (<> [child])
  pure (Focus mv child)

modify' :: IORef a -> (a -> a) -> App ()
modify' ref f = liftIO $ atomicModifyIORef' ref (\x -> (f x, ()))
