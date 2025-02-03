module Yesterday.Types where

import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Text as T

data Function = Function
  { funParameters :: [Variable],
    funResult :: Variable,
    funClauses :: [Clause]
  }
  deriving (Show)

newtype Variable = Variable
  {varName :: T.Text}
  deriving (Show, Eq, Ord)

data Clause = Clause Expr [Expr]
  deriving (Show)

data Expr
  = ELit Value
  | EHist Expr [HistOp]
  | EGets Expr Expr
  | EPlusGets Expr Expr
  | EDeref Expr
  | EAdd Expr Expr
  | EMul Expr Expr
  | ESub Expr Expr
  | EDiv Expr Expr
  | EEq Expr Expr
  | ENEq Expr Expr
  | ELT Expr Expr
  | ELE Expr Expr
  | EGT Expr Expr
  | EGE Expr Expr
  | ECall Expr [Expr]
  | EAction Action Expr
  | EAnd Expr Expr
  | EOr Expr Expr
  | ENull Variable
  | EInterrobang Expr
  | EHistLit Expr
  deriving (Show)

data HistOp
  = HistParent Expr
  | HistChild Expr
  deriving (Show)

data Action
  = -- | Print to stdout
    WriteStdout
  | -- | Print to stderr
    WriteStderr
  deriving (Show)

data Value
  = -- | A boolean literal
    BoolLit Bool
  | -- | An integer literal
    IntegerLit Integer
  | -- | A string literal
    StringLit T.Text
  | -- | A variable (history)
    Var Variable
  | -- | A function
    Func Function
  | -- | Uninterseting side effect
    SideEffect
  deriving (Show)

data History = History
  { _parent :: Maybe (History, IORef Int),
    _children :: IORef [History],
    _payload :: Maybe (Either Focus Value)
  }

data Focus = Focus
  { _var :: Maybe Variable,
    _history :: History
  }

data Frame = Frame
  { _currentFunc :: Function,
    _foci :: IORef (M.Map Variable Focus)
  }
