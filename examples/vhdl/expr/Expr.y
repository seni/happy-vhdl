{
module Expr where
import Char
}

%name expr
%tokentype { Token }



%token
'n' {Token_num}
'+' {Token_plus}
'*' {Token_mult}
'(' {Token_left_paren}
')' {Token_right_paren}

%left '+'
%left '*'

%%

Exp :: {Exp}
Exp : 'n' { N }
    | Exp '+' Exp { Plus $1 $3 }
    | Exp '*' Exp { Mult $1 $3 }
    | '(' Exp ')' { $2 }

--Exp : Exp '+' Term { Plus $1 $3 }
--    | Term         { $1 }

--Term : Term '*' Factor { Mult $1 $3 }
--     | Factor          { $1 }

--Factor : '(' Exp ')' { $2 }
--       | 'n'         { N }

{

happyError :: [Token] -> a
happyError _ = error ("Parse error\n")

data Exp = N | Plus Exp Exp | Mult Exp Exp
  deriving Show

data Token = Token_num | Token_plus | Token_mult
           | Token_left_paren | Token_right_paren

lexer :: String -> [Token]
lexer [] = []
lexer ('n':cs) = Token_num         : lexer cs
lexer ('+':cs) = Token_plus        : lexer cs
lexer ('*':cs) = Token_mult        : lexer cs
lexer ('(':cs) = Token_left_paren  : lexer cs
lexer (')':cs) = Token_right_paren : lexer cs
lexer ( _ :cs) = lexer cs

run :: String -> Exp
run = expr . lexer

}
