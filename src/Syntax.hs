-- DEFINES AST


module Syntax where

import qualified Data.Map.Strict as M

newtype Var = Var String
  deriving (Eq, Ord, Show)

data Node
  = LitStr String
  | LitInt Int
  | URI String
  deriving (Eq, Ord, Show)

data Triple = Triple Node Node Node
  deriving (Eq, Ord, Show)

data Term
  = TVar Var
  | TNode Node
  deriving (Eq, Ord, Show)

data Pattern = Pattern Term Term Term
  deriving (Eq, Ord, Show)

data Expr
  = EVar Var
  | ENode Node
  deriving (Eq, Ord, Show)

data BoolExpr
  = BEq Expr Expr
  | BNe Expr Expr
  | BGe Expr Expr
  | BGt Expr Expr
  | BLt Expr Expr
  | BAnd BoolExpr BoolExpr
  | BOr BoolExpr BoolExpr
  | BNot BoolExpr
  deriving (Eq, Ord, Show)

data AggOp = AggMax | AggCount
  deriving (Eq, Ord, Show)

data AggBind = AggBind Var AggOp Expr
  deriving (Eq, Ord, Show)

data TripleTemplate = TripleTemplate Term Term Term
  deriving (Eq, Ord, Show)

data Query = Query
  { qSources   :: [String]
  , qPatterns  :: [Pattern]
  , qFilter    :: Maybe BoolExpr
  , qGroupBy   :: [Var]
  , qAggs      :: [AggBind]
  , qConstruct :: [TripleTemplate]
  } deriving (Eq, Ord, Show)

type Binding = M.Map Var Node