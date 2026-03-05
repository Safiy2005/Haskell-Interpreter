{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$alphanum = [a-zA-Z0-9]

tokens :-

  $white+                       ; -- Skip whitespace
  "--".* ; -- Simple comments

  -- Keywords (Case-insensitive-ish)
  [Ll][Oo][Aa][Dd]              { \s -> TokenLoad }
  [Ss][Ee][Ll][Ee][Cc][Tt]      { \s -> TokenSelect }
  [Ww][Hh][Ee][Rr][Ee]          { \s -> TokenWhere }
  "@prefix"                     { \s -> TokenPrefixDecl }
  "@base"                       { \s -> TokenBaseDecl }

  -- RDF Components
  "<" [^ \> \n]* ">"            { \s -> TokenURI (init (tail s)) }
  \" [^\"]* \"                  { \s -> TokenStr (init (tail s)) }
  $digit+                       { \s -> TokenInt (read s) }
  "?" $alpha $alphanum* { \s -> TokenVar (tail s) } -- Variables like ?x

  -- Prefixed Names (e.g., foaf:person)
  $alpha $alphanum* ":" $alpha $alphanum* { \s -> TokenPrefixedName s }

  -- Punctuation
  "."                           { \s -> TokenDot }
  ";"                           { \s -> TokenSemi }
  ","                           { \s -> TokenComma }
  "{"                           { \s -> TokenLBrace }
  "}"                           { \s -> TokenRBrace }
  "="                           { \s -> TokenEq }

{
-- The Token data type
data Token
  = TokenLoad
  | TokenSelect
  | TokenWhere
  | TokenPrefixDecl
  | TokenBaseDecl
  | TokenURI String
  | TokenStr String
  | TokenInt Integer
  | TokenVar String
  | TokenPrefixedName String
  | TokenDot
  | TokenSemi
  | TokenComma
  | TokenLBrace
  | TokenRBrace
  | TokenEq
  deriving (Eq, Show)
}