-- This file is heavily based on the template from
-- https://github.com/dagit/happy-plus-alex

{
{-# OPTIONS -w  #-}
module Frontend.Lexer where

import Control.Monad (liftM)
import Data.Char (chr)
import Prelude hiding (lex)
import SPCF.AST (Label)
}
%wrapper "monadUserState"
$digit = 0-9
$alpha = [A-Za-z]
tokens :-

  "#".*                         ;
  $white+                       ;
  "->"                          { lex' TokenArrow }
  "=>"                          { lex' TokenDoubleArrow }
  Bool                          { lex' TokenBoolTy }
  Nat                           { lex' TokenNatTy }
  true                          { lex' $ TokenBool True }
  false                         { lex' $ TokenBool False }
  error1                        { lex' TokenError1 }
  error2                        { lex' TokenError2 }
  succ                          { lex' TokenSucc }
  pred                          { lex' TokenPred }
  catch                         { lex' TokenCatch }
  fix                           { lex' TokenFix }
  iszero                        { lex' TokenIszero }
  if0                           { lex' TokenIf }
  then                          { lex' TokenThen }
  else                          { lex' TokenElse }
  eval                          { lex' TokenEval }
  $digit+                       { lex (TokenNat . read) }
  \=                            { lex' TokenEq }
  \\                            { lex' TokenBackslash }
  \(                            { lex' TokenLParen }
  \)                            { lex' TokenRParen }
  \:                            { lex' TokenColon }
  \;                            { lex' TokenSemicolon }
  -- eof                        { lex' TokenEOF }
  $alpha [$alpha $digit \_ \']* { lex (TokenId) }

{
-- To improve error messages, We keep the path of the file we are
-- lexing in our own state.
data AlexUserState = AlexUserState { filePath :: FilePath }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>"

getFilePath :: Alex FilePath
getFilePath = liftM filePath alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath = alexSetUserState . AlexUserState

-- The token type, consisting of the source code position and a token class.
data Token = Token AlexPosn TokenClass
  deriving (Show)

data TokenClass =
  TokenId Label
  | TokenLParen
  | TokenRParen
  | TokenColon
  | TokenSemicolon
  | TokenBackslash
  | TokenBoolTy
  | TokenNatTy
  | TokenArrow
  | TokenDoubleArrow
  | TokenBool Bool
  | TokenError1
  | TokenError2
  | TokenNat Int
  | TokenEq
  | TokenSucc
  | TokenPred
  | TokenCatch
  | TokenIszero
  | TokenFix
  | TokenIf
  | TokenThen
  | TokenElse
  | TokenEval
  | TokenEOF
    deriving (Eq, Show)

-- For nice parser error messages.
unLex :: TokenClass -> String
unLex (TokenId id)         = show id
unLex TokenLParen          = "("
unLex TokenRParen          = ")"
unLex TokenColon           = ":"
unLex TokenSemicolon       = ";"
unLex TokenBackslash       = "\\"
unLex TokenBoolTy          = "Bool"
unLex TokenNatTy           = "Nat"
unLex TokenArrow           = "->"
unLex TokenDoubleArrow     = "=>"
unLex (TokenBool b)        = show b
unLex (TokenNat i)         = show i
unLex TokenError1          = "error1"
unLex TokenError2          = "error2"
unLex TokenEq              = "="
unLex TokenSucc            = "succ"
unLex TokenPred            = "pred"
unLex TokenCatch           = "catch"
unLex TokenIszero          = "iszero"
unLex TokenFix             = "fix"
unLex TokenIf              = "if0"
unLex TokenThen            = "then"
unLex TokenElse            = "else"
unLex TokenEval            = "eval"
unLex TokenEOF             = "<EOF>"

alexEOF :: Alex Token
alexEOF = do
  (p,_,_,_) <- alexGetInput
  return $ Token p TokenEOF

-- Unfortunately, we have to extract the matching bit of string
-- ourselves...
lex :: (String -> TokenClass) -> AlexAction Token
lex f = \(p,_,_,s) i -> return $ Token p (f (take i s))

-- For constructing tokens that do not depend on
-- the input
lex' :: TokenClass -> AlexAction Token
lex' = lex . const

-- We rewrite alexMonadScan' to delegate to alexError' when lexing fails
-- (the default implementation just returns an error message).
alexMonadScan' :: Alex Token
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (p, _, _, s) ->
        alexError' p ("lexical error at character '" ++ take 1 s ++ "'")
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

-- Signal an error, including a commonly accepted source code position.
alexError' :: AlexPosn -> String -> Alex a
alexError' (AlexPn _ l c) msg = do
  fp <- getFilePath
  alexError (fp ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ msg)

-- A variant of runAlex, keeping track of the path of the file we are lexing.
runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a fp input = runAlex input (setFilePath fp >> a)
}