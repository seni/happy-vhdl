{
module Prolog_list where
import Char
}

%name prolog_list

%tokentype { Token }


%token
'['                  { Token_open_list }
']'                  { Token_close_list }
','                  { Token_comma }
'|'                  { Token_ht_sep }
Integer_constant     { Token_Integer_constant }
Hexadecimal_constant { Token_Hexadecimal_constant }
Anonymous_Variable   { Token_Anonymous_Variable }
Named_Variable       { Token_Named_Variable }

%%

Term :: { PL }
Term : Atom          { $1 }
     | Variable      { $1 }
     | Integer       { $1 }
     | '[' Items ']' { $2 }

Items : Exp ',' Items { Append $1 $3 }
      | Exp '|' Exp   { Append $1 $3 }
      | Exp           { $1 }

Exp : Term { $1 }

Integer : -- [ Layout_text_sequence ]
          Integer_token { $1 }

Variable : -- [ Layout_text_sequence ]
           Variable_token { $1 }

Variable_token : Anonymous_Variable { A }
               | Named_Variable     { N }

Integer_token : Integer_constant     { I }
              | Hexadecimal_constant { H }

Atom : Empty_list { $1 }

Empty_list : '[' ']' { Empty }

{

happyError :: [Token] -> a
happyError _ = error ("Parse error\n")

data PL = N | A | I | H | Append PL PL | Empty
  deriving Show

data Token = Token_open_list | Token_close_list
           | Token_comma | Token_ht_sep
           | Token_Integer_constant | Token_Hexadecimal_constant
           | Token_Anonymous_Variable | Token_Named_Variable

lexer :: String -> [Token]
lexer [] = []
lexer ('[':cs) = Token_open_list            : lexer cs
lexer (']':cs) = Token_close_list           : lexer cs
lexer (',':cs) = Token_comma                : lexer cs
lexer ('|':cs) = Token_ht_sep               : lexer cs
lexer ('i':cs) = Token_Integer_constant     : lexer cs
lexer ('x':cs) = Token_Hexadecimal_constant : lexer cs
lexer ('a':cs) = Token_Anonymous_Variable   : lexer cs
lexer ('n':cs) = Token_Named_Variable       : lexer cs
lexer ( _ :cs) = lexer cs

run :: String -> PL
run = prolog_list . lexer

}
