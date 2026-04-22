{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [A-Za-z]
$alphanum = [A-Za-z0-9_]

@name = $alpha $alphanum*
@var  = \? $alpha $alphanum*
@uri  = \< [^ \> \n]* \>
@str  = \" [^\"]* \"

tokens :-

  $white+                           ;
  "#" [^\n]*                       ;

  from                              { \_ -> TokFrom }
  match                             { \_ -> TokMatch }
  where                             { \_ -> TokWhere }
  construct                         { \_ -> TokConstruct }
  group                             { \_ -> TokGroup }
  by                                { \_ -> TokBy }
  aggregate                         { \_ -> TokAggregate }
  max                               { \_ -> TokMax }
  and                               { \_ -> TokAnd }
  or                                { \_ -> TokOr }
  not                               { \_ -> TokNot }
  count                             { \_ -> TokCount}
  min                               { \_ -> TokMin}
  as                                { \_ -> TokAs}
  exists                            { \_ -> TokExists}
  
  "!="                              { \_ -> TokNe }
  ">="                              { \_ -> TokGe }
  ">"                               { \_ -> TokGt }
  "<"                               { \_ -> TokLt }
  ","                               { \_ -> TokComma }
  "="                               { \_ -> TokEq }
  "("                               { \_ -> TokLParen }
  ")"                               { \_ -> TokRParen }
  ":"                               { \_ -> TokColon}
  

  @var                              { \s -> TokVar (tail s) }
  @uri                              { \s -> TokURI (init (tail s)) }
  @str                              { \s -> TokString (init (tail s)) }
  $digit+                           { \s -> TokInt (read s) }
  @name                             { \s -> TokName s }

{
data Token
  = TokFrom
  | TokMatch
  | TokWhere
  | TokConstruct
  | TokGroup
  | TokBy
  | TokAggregate
  | TokMax
  | TokMin
  | TokAnd
  | TokOr
  | TokNot
  | TokCount
  | TokComma
  | TokEq
  | TokNe
  | TokGe
  | TokGt
  | TokLt
  | TokLParen
  | TokRParen
  | TokVar String
  | TokName String
  | TokURI String
  | TokString String
  | TokInt Int
  | TokAs
  | TokExists
  | TokColon
  deriving (Eq, Show)
}