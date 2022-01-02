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

%%

Void : { Void }
     | Let { $1 }

Let : Seq { $1 }
    | val name '=' App ';' Let { Let $2 $4 $6 }
    | fn name name '{' Void '}' Let { Let $2 (Abs $3 $5) $7 }
    | fn name '(' name ')' '{' Void '}' Let { Let $2 (Abs $4 $7) $9 }

Seq : App { $1 }
    | Seq ';' { Seq $1 Void }
    | Seq ';' App { Seq $1 $3 }

App : Exp { $1 }
    | App Exp { App $1 $2 }

Exp : name { Exp.Var $1 }
    | fn name '{' Void '}' { Abs $2 $4 }
    | fn '(' name ')' '{' Void '}' { Abs $3 $6 }
    | '(' Seq ')' { $2 }

{
lexer :: (Token -> Lex a) -> Lex a
lexer = (Lex.getToken >>=)
}
