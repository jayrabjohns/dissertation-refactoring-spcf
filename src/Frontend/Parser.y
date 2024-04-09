-- This grammar file is heavily based on
-- 1) The template found here https://github.com/dagit/happy-plus-alex
-- 2) The STLC Yacc grammar included with TAPL by Benjamin Pierce

{
{-# OPTIONS -w #-}
module Frontend.Parser( parseProg ) where

import Frontend.Lexer
import SPCF -- (Term(..), Type(..), Label, termInfo)
}

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TokenEOF }
%error { happyError }

%token
  arrow        { Token $$ TokenArrow }
  doublearrow  { Token $$ TokenDoubleArrow }
  bool         { Token $$ TokenBoolTy }
  nat          { Token $$ TokenNatTy }
  true         { Token $$ (TokenBool True) }
  false        { Token $$ (TokenBool False) }
  error1       { Token $$ TokenError1 }
  error2       { Token $$ TokenError2 }
  natVal       { Token _ (TokenNat _) }
  succ         { Token $$ TokenSucc }
  pred         { Token $$ TokenPred }
  catch        { Token $$ TokenCatch }
  iszero       { Token $$ TokenIszero }
  fix          { Token $$ TokenFix }
  if           { Token $$ TokenIf }
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
  -- eof          { Token $$ TokenEOF }

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
  Commands { SPCF.Prog { SPCF.pinfo_of = AlexPn 0 0 0, SPCF.prog_of = $1 } }

-- Atomic types
AType :
  nat { SPCF.Base }
  | '(' Type ')' { $2 }

ArrowType :
  AType arrow ArrowType { (SPCF.:->) $1 $3 }
  | AType { $1 }

Type :
  ArrowType { $1 }

Term :
  AppTerm { $1 }
  | if Term then Term else Term { SPCF.If0 $1 $2 $4 $6 }
  | '\\' id TyBinder doublearrow Term {
    case $2 of
      Token _ (TokenId id) ->
        SPCF.Lambda $1 id $3 $5 }
  -- | Term Term %prec APP    { Ast.TmApp () $1 $2 }

AppTerm :
  ATerm { $1 }
  | AppTerm ATerm { SPCF.Apply (termInfo $1) $1 $2 }
  | succ ATerm { SPCF.Succ $1 $2 }
  | pred ATerm { SPCF.Pred $1 $2 }
  | fix ATerm { SPCF.YComb $1 $2 }
  | catch ATerm { SPCF.Catch $1 $2 }

-- Atomic terms
ATerm :
  '(' Term ')' { $2 }
  | natVal {
    case $1 of
      Token fi (TokenNat n) ->
        SPCF.Numeral fi n }
        --foldr (\_ acc -> SPCF.Succ fi acc) (SPCF.Numeral fi 0) [1..n] }
  | error1 { SPCF.Error $1 SPCF.Error1 }
  | error2 { SPCF.Error $1 SPCF.Error2 }
  | id {
    case $1 of
      Token fi (TokenId id) ->
        SPCF.Variable fi id }

Command :
  id Binder {
    case $1 of
      Token fi (TokenId id) ->
        SPCF.CBind fi id $2 }
  | eval Term { SPCF.CEval $1 $2 }

TyBinder :
  ':' Type { $2 }

Binder :
  '=' Term { $2 }

Commands :
  Commands ';' Commands { $1 ++ $3 }
  -- | Commands ';' { $1 }
  | Command { [$1] }
  | {- empty -} { [] }

{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) =
  alexError' p ("parse error at token '" ++ unLex t ++ "'")


parseProg :: FilePath -> String -> Either String (SPCF.Prog AlexPosn)
parseProg = runAlex' parse
}