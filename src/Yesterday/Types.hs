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

data Clause = Clause Expr [Action]
  deriving (Show)

data Expr
  = ELit Value
  | EHist Expr [HistOp]
  | EDeref Expr
  | EAdd Expr Expr
  | EMul Expr Expr
  | ESub Expr Expr
  | EDiv Expr Expr
  | EEq Expr Expr
  | ENEq Expr Expr
  | ECall Expr [Expr]
  deriving (Show)

data HistOp
  = HistParent Expr
  | HistChild Expr
  deriving (Show)

data Action
  = -- | Computation evolution.
    Gets Variable Expr
  | -- | Print to stdout
    WriteStdout Expr
  | -- | Print to stderr
    WriteStderr Expr
  deriving (Show)

data Value
  = -- | The _ wildcard matching anything but the empty history
    AnyLit
  | -- | A boolean literal
    BoolLit Bool
  | -- | An integer literal
    IntegerLit Integer
  | -- | A string literal
    StringLit T.Text
  | -- | A variable (history)
    Var Variable
  | -- | A function
    Func Function
  deriving (Show)

data History = History
  { _var :: Maybe Variable,
    _parent :: Maybe History,
    _children :: IORef [History],
    _payload :: Maybe (Either History Value)
  }

data Frame = Frame
  { _currentFunc :: Function,
    _histories :: IORef (M.Map Variable History)
  }
