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
  ';' { Semicolon }
  '=' { Equals }
  fn { Fn }
  val { Val }
  var { Token.Var }
  switch { Token.Switch }
  case { Token.Case }
  name { Name $$ }

%nonassoc '{' '}' '(' ')' fn val var switch case name
%left '.'
%right '='
%left ','
%left ';'
%left App
%left Let
%left Seq

%%

Void : { Void }
     | Exp { $1 }
     | Exp ';' { Seq $1 Void }

Exp : name { Exp.Var $1 }
    | fn name '{' Void '}' { Abs $2 $4 }
    | fn '(' name ')' '{' Void '}' { Abs $3 $6 }
    | Exp Exp %prec App { App $1 $2 }
    | val name '=' Exp ';' Exp %prec Let { Let $2 $4 $6 }
    | Exp ';' Exp %prec Seq { Seq $1 $3 }

{
lexer :: (Token -> Lex a) -> Lex a
lexer = (Lex.getToken >>=)
}
