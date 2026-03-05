{
module Parser where
import Lexer
import Syntax
}

%name parseRQL
%tokentype { Token }
%error { parseError }

%token
    load    { TokenLoad }
    select  { TokenSelect }
    where   { TokenWhere }
    prefix  { TokenPrefixDecl }
    uri     { TokenURI $$ }
    str     { TokenStr $$ }
    int     { TokenInt $$ }
    var     { TokenVar $$ }
    '.'     { TokenDot }
    '{'     { TokenLBrace }
    '}'     { TokenRBrace }

%%

-- A program is a list of commands
Program : Commands { reverse $1 }

Commands : Commands Command { $2 : $1 }
         | Command          { [$1] }

Command : load str '.'                     { Load $2 }
        | prefix var uri '.'               { Prefix $2 $3 }
        | select VarList where '{' TripleList '}' '.' { Select $2 $5 }

-- List of variables: ?a ?b ?c
VarList : var VarList { $1 : $2 }
        | var         { [$1] }

-- List of triples: ?s ?p ?o . ?s2 ?p2 ?o2 .
TripleList : TriplePattern '.' TripleList { $1 : $3 }
           | TriplePattern '.'            { [$1] }

TriplePattern : Term Term Term { TriplePattern $1 $2 $3 }

Term : var { Var $1 }
     | uri { URI $1 }
     | str { LitString $1 }
     | int { LitInt $1 }

{
parseError :: [Token] -> a
parseError tokens = error $ "Parse error at tokens: " ++ show tokens
}