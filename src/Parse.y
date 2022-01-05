{
{-# LANGUAGE ImportQualifiedPost #-}
module Parse
  ( parse
  ) where

import Exp
import Lex (Lex)
import Lex qualified
import Token
}

%name parse
%tokentype { Token }
%monad { Lex }
%lexer { lexer } { EOF }
%error { Lex.throwError }

%token
  '{' { LeftBrace }
  '}' { RightBrace }
  '(' { LeftParen }
  ')' { RightParen }
  '.' { Period }
  ',' { Comma }
  ':' { Colon }
  ';' { Semicolon }
  '=' { Equals }
  '\\' { Backslash }
  fn { Fn }
  val { Val }
  var { Token.Var }
  struct { Token.Struct }
  switch { Token.Switch }
  case { Token.Case }
  enum { Token.Enum }
  name { Name $$ }

%%

Void : { Void }
     | Let { $1 }

Let : FieldName { $1 }
    | FieldName ';' { Seq $1 Void }
    | FieldName ';' Let { Seq $1 $3 }
    | val name '=' App ';' Let { Let $2 $4 $6 }
    | fn name Names '{' Void '}' Let { Let $2 (foldr Abs $5 $3) $7 }

FieldName : App { $1 }
          | FieldName '.' name { Field $1 $3 }

App : Exp { $1 }
    | App Exp { App $1 $2 }

Exp : name { Exp.Var $1 }
    | '\\' Names '{' Void '}' { foldr Abs $4 $2 }
    | '(' Void ')' { $2 }
    | struct MaybeName '{' Fields '}' { Exp.Struct $2 $4 }
    | switch Void '{' Case Cases '}' { Exp.Switch $2 $4 $5 }
    | enum name { Exp.Enum $2 }

Names : RevNames { reverse $1 }

RevNames : name { [$1] }
         | RevNames name { $2:$1 }

MaybeName : { Nothing }
          | name { Just $1 }

Fields : RevFields { reverse $1 }

RevFields : { [] }
          | RevFields Field { $2:$1 }

Field : name ':' Exp { ($1, $3) }

Cases : RevCases { reverse $1 }

RevCases : { [] }
         | RevCases Case { $2:$1 }

Case : case name ':' Void { ($2, $4) }

{
lexer :: (Token -> Lex a) -> Lex a
lexer = (Lex.getToken >>=)
}
