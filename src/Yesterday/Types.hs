module Yesterday.Types where

import qualified Data.Text as T
import Data.Tree

data Function = Function
  { funName :: T.Text,
    parameters :: [Variable],
    results :: [Variable],
    clauses :: [Clause]
  }
  deriving (Show)

newtype Variable = Variable
  {varName :: T.Text}
  deriving (Show)

data Clause = Clause Expr [Action]
  deriving (Show)

data Expr
  = ELit Value
  | EAdd Expr Expr
  | EMul Expr Expr
  | ESub Expr Expr
  | EDiv Expr Expr
  | EEq Expr Expr
  | ENEq Expr Expr
  | ECall Expr [Expr]
  deriving (Show)

data HistOp
  = HistParent Int
  | HistChild Int
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
    BoolLit
  | -- | An integer literal
    IntegerLit
  | -- | A double literal
    DoubleLit
  | -- | A string literal
    StringLit
  | -- | A variable, possibly followed by history operations
    VarLiteral Variable [HistOp]
  | -- | A dereferenced variable, e.g. @x
    VarDeref
  | -- | A computation history
    History [Tree Value]
  deriving (Show)
