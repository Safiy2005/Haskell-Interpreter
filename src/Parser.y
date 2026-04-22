{
module Parser
  ( parseProgram
  ) where

import Lexer
import Syntax
}

%name parseQuery
%tokentype { Token }
%error { parseError }

%token
  from       { TokFrom }
  match      { TokMatch }
  where      { TokWhere }
  construct  { TokConstruct }
  group      { TokGroup }
  by         { TokBy }
  aggregate  { TokAggregate }
  max        { TokMax }
  min        { TokMin }
  and        { TokAnd }
  or         { TokOr }
  not        { TokNot }
  count      { TokCount}
  ','        { TokComma }
  '='        { TokEq }
  ne         { TokNe }
  ge         { TokGe }
  gt         { TokGt }
  lt         { TokLt }
  '('        { TokLParen }
  ')'        { TokRParen }
  var        { TokVar $$ }
  name       { TokName $$ }
  uri        { TokURI $$ }
  str        { TokString $$ }
  int        { TokInt $$ }
  as         { TokAs }
  exists     { TokExists }
  ':'        { TokColon }


%right or
%right and
%right not

%%

Query :: { Query }
Query
  : from SourceList match PatternList MaybeWhere MaybeGroup MaybeAggregate construct TemplateList
      { Query $2 $4 $5 $6 $7 $9 }

SourceList :: { [SrcSpec] }
SourceList
  : SrcSpec                     { [$1] }
  | SourceList ',' SrcSpec      { $1 ++ [$3] }

SrcSpec :: {SrcSpec}
SrcSpec
  : name as name                { SrcSpec $1 (SrcAlias $3) }
  | name                        { SrcSpec $1 (SrcAlias $1) }

PatternList :: { [Pattern] }
PatternList
  : Pattern                  { [$1] }
  | PatternList ',' Pattern  { $1 ++ [$3] }

Pattern :: { Pattern }
Pattern
  : Term Term Term           { Pattern Nothing $1 $2 $3 }
  | name ':' Term Term Term  { Pattern (Just (SrcAlias $1)) $3 $4 $5}

MaybeWhere :: { Maybe BoolExpr }
MaybeWhere
  :                          { Nothing }
  | where BoolExpr           { Just $2 }

MaybeGroup :: { [Var] }
MaybeGroup
  :                          { [] }
  | group by VarList         { $3 }

MaybeAggregate :: { [AggBind] }
MaybeAggregate
  :                          { [] }
  | aggregate AggList        { $2 }

VarList :: { [Var] }
VarList
  : var                      { [Var $1] }
  | VarList ',' var          { $1 ++ [Var $3] }

AggList :: { [AggBind] }
AggList
  : AggBind                  { [$1] }
  | AggList ',' AggBind      { $1 ++ [$3] }

AggBind :: { AggBind }
AggBind
  : var '=' max   '(' Expr ')' { AggBind (Var $1) AggMax   $5 }
  | var '=' min   '(' Expr ')' { AggBind (Var $1) AggMin   $5 }
  | var '=' count '(' Expr ')' { AggBind (Var $1) AggCount $5 }

TemplateList :: { [TripleTemplate] }
TemplateList
  : Template                 { [$1] }
  | TemplateList ',' Template { $1 ++ [$3] }

Template :: { TripleTemplate }
Template
  : Term Term Term           { TripleTemplate $1 $2 $3 }

Term :: { Term }
Term
  : var                      { TVar (Var $1) }
  | uri                      { TNode (URI $1) }
  | str                      { TNode (LitStr $1) }
  | int                      { TNode (LitInt $1) }

Expr :: { Expr }
Expr
  : var                      { EVar (Var $1) }
  | uri                      { ENode (URI $1) }
  | str                      { ENode (LitStr $1) }
  | int                      { ENode (LitInt $1) }

BoolExpr :: { BoolExpr }
BoolExpr
  : Expr '=' Expr            { BEq $1 $3 }
  | Expr ne Expr             { BNe $1 $3 }
  | Expr ge Expr             { BGe $1 $3 }
  | Expr gt Expr             { BGt $1 $3 }
  | Expr lt Expr             { BLt $1 $3 }
  | BoolExpr and BoolExpr    { BAnd $1 $3 }
  | BoolExpr or BoolExpr     { BOr $1 $3 }
  | not BoolExpr             { BNot $2 }
  | '(' BoolExpr ')'         { $2 }
  | exists name ':' Term Term Term
                             { BExists (SrcAlias $2) (Pattern (Just (SrcAlias $2)) $4 $5 $6)}

{
parseError :: [Token] -> a
parseError [] = error "Parse error at end of input"
parseError ts = error ("Parse error near tokens: " ++ show (take 3 ts))

parseProgram :: String -> Query
parseProgram = parseQuery . alexScanTokens
}