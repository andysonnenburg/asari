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
  struct { Token.Struct }
  switch { Token.Switch }
  name { Name $$ }

%%

Void : { Void }
     | Let { $1 }

NonVoid : NonVoidLet { $1 }

Let : Seq { $1 }
    | val name '=' App ';' Let { Let $2 $4 $6 }
    | fn name name '{' Void '}' Let { Let $2 (Abs $3 $5) $7 }
    | fn name '(' name ')' '{' Void '}' Let { Let $2 (Abs $4 $7) $9 }

NonVoidLet : NonVoidSeq { $1 }
           | val name '=' App ';' NonVoidLet { Let $2 $4 $6 }

Seq : NonVoidSeq { $1 }
    | NonVoidSeq ';' { Seq $1 Void }

NonVoidSeq : Field { $1 }
           | NonVoidSeq ';' Field { Seq $1 $3 }

Field : App { $1 }
      | Field '.' name { Field $1 $3 }

App : Exp { $1 }
    | App Exp { App $1 $2 }

Exp : name { Exp.Var $1 }
    | fn name '{' Void '}' { Abs $2 $4 }
    | fn '(' name ')' '{' Void '}' { Abs $3 $6 }
    | '(' NonVoid ')' { $2 }
    | struct '{' Labels '}' { Exp.Struct $3 }
    | switch NonVoid '{' Label Labels '}' { Exp.Switch $2 $4 $5 }

Labels : RevLabels { reverse $1 }

RevLabels : { [] }
          | RevLabels Label { $2 : $1 }

Label : name '{' Void '}' { ($1, $3) }

{
lexer :: (Token -> Lex a) -> Lex a
lexer = (Lex.getToken >>=)
}
