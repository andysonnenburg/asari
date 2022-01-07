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
  case { Case }
  default { Default }
  enum { Token.Enum }
  name { Name $$ }

%%

Void : { Void }
     | Let { $1 }

Let : Block { $1 }
    | Block ';' { Seq $1 Void }
    | Block ';' Let { Seq $1 $3 }
    | val name '=' Block ';' Let { Let $2 $4 $6 }
    | fn name Names '{' Void '}' Let { Let $2 (foldr Abs $5 $3) $7 }

Block : FieldName { $1 }
      | Block FieldName { App $1 $2 }
      | Block '{' Void '}' { App $1 (Block $3) }

App : FieldName { $1 }
    | App FieldName { App $1 $2 }

FieldName : Exp { $1 }
          | FieldName '.' name { Field $1 $3 }

Exp : name { Exp.Var $1 }
    | '\\' Names '{' Void '}' { foldr Abs $4 $2 }
    | '(' Void ')' { $2 }
    | struct MaybeName '{' Fields '}' { Exp.Struct $2 $4 }
    | switch App '{' Case Cases MaybeDefault '}' { Exp.Switch $2 $4 $5 $6 }
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

MaybeDefault : { Nothing }
             | Default { Just $1 }

Default : default ':' Void { $3 }

{
lexer :: (Token -> Lex a) -> Lex a
lexer = (Lex.getToken >>=)
}
