-- Based on https://github.com/dagit/happy-plus-alex
-- Based on https://github.com/bagnalla/PCF

{
{-# OPTIONS -w #-}
module Frontend.Parser(parseProg) where

import Frontend.Lexer
import SPCF.AST (Error(..), Label, Term(..), Type(..), termInfo)
import SPCF.Interpreter(Statement(..), Program(..),)
}

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TokenEOF }
%error { happyError }

%token
  arrow        { Token $$ TokenArrow }
  doublearrow  { Token $$ TokenDoubleArrow }
  nat          { Token $$ TokenNatTy }
  error1       { Token $$ TokenError1 }
  error2       { Token $$ TokenError2 }
  natVal       { Token _ (TokenNat _) }
  succ         { Token $$ TokenSucc }
  pred         { Token $$ TokenPred }
  catch        { Token $$ TokenCatch }
  fix          { Token $$ TokenFix }
  if           { Token $$ TokenIf0 }
  then         { Token $$ TokenThen }
  else         { Token $$ TokenElse }
  eval         { Token $$ TokenEval }
  id           { Token _ (TokenId _) }
  '='          { Token $$ TokenEq }
  '\\'         { Token $$ TokenBackslash }
  '('          { Token $$ TokenLParen }
  ')'          { Token $$ TokenRParen }
  ':'          { Token $$ TokenColon }
  ';'          { Token $$ TokenSemicolon }

%left ';'
%nonassoc '='
%right arrow doublearrow
%right succ pred iszero
%left succ pred
%nonassoc iszero fix
%nonassoc true false natVal id
%left APP
%nonassoc '(' ')'
%nonassoc ':'
%%

Prog :
  Statements { SPCF.Interpreter.Prog { SPCF.Interpreter.pinfo_of = AlexPn 0 0 0, SPCF.Interpreter.prog_of = $1 } }

-- Atomic terms
AType :
  nat { SPCF.AST.Base }
  | '(' Type ')' { $2 }

ArrowType :
  AType arrow ArrowType { (SPCF.AST.:->) $1 $3 }
  | AType { $1 }

Type :
  ArrowType { $1 }

Term :
  AppTerm { $1 }
  | if Term then Term else Term { SPCF.AST.If0 $1 $2 $4 $6 }
  | '\\' id TyBinder doublearrow Term {
    case $2 of
      Token _ (TokenId id) -> SPCF.AST.Lambda $1 id $3 $5 }

AppTerm :
  ATerm { $1 }
  | AppTerm ATerm { SPCF.AST.Apply (termInfo $1) $1 $2 }
  | succ ATerm { SPCF.AST.Succ $1 $2 }
  | pred ATerm { SPCF.AST.Pred $1 $2 }
  | fix ATerm { SPCF.AST.YComb $1 $2 }
  | catch ATerm { SPCF.AST.Catch $1 $2 }

-- Atomic terms
ATerm :
  '(' Term ')' { $2 }
  | natVal {
    case $1 of
      Token info (TokenNat n) -> SPCF.AST.Numeral info n }
  | error1 { SPCF.AST.Error $1 SPCF.AST.Error1 }
  | error2 { SPCF.AST.Error $1 SPCF.AST.Error2 }
  | id {
    case $1 of
      Token info (TokenId id) -> SPCF.AST.Variable info id }

Statement :
  id Binder {
    case $1 of
      Token info (TokenId id) -> SPCF.Interpreter.Declare info id $2 }
  | eval Term { SPCF.Interpreter.Evaluate $1 $2 }

TyBinder :
  ':' Type { $2 }

Binder :
  '=' Term { $2 }

Statements :
  Statements ';' Statements { $1 ++ $3 }
  | Statement { [$1] }
  | {- empty -} { [] }

{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) =
  alexError' p ("parse error at token '" ++ unLex t ++ "'")


parseProg :: FilePath -> String -> Either String (SPCF.Interpreter.Program AlexPosn)
parseProg = runAlex' parse
}