module SPCF.Constants (apply, (<+), (+>), addLeft, addRight, addLeftTerm, addRightTerm, info) where

import Frontend.Lexer (AlexPosn (..))
import SPCF.AST
import SPCF.Evaluation

info :: AlexPosn
info = AlexPn 0 0 0

-- Apply any number of arguments to a term.
--   A helper function for the construction of nested applications.
apply :: Term AlexPosn -> [Term AlexPosn] -> Term AlexPosn
apply term [] = term
apply term (arg : args) = apply (Apply (termInfo term) term arg) args

-- Prototypical divergent expression
omega :: Term AlexPosn
omega = YComb info $ Lambda info "x" undefined (Variable info "x")

-- Addition operator.
--   Recurses on the left argument, also known as left addition.
--   This is a partial function and will error on unsuccessful evaluations.
(<+) :: Term AlexPosn -> Term AlexPosn -> Term AlexPosn
(<+) lhs rhs = either error id val where val = interpret (addLeft lhs rhs)

-- Addition operator.
--   Recurses on the right argument, also known as right addition.
--   This is a partial function and will error on unsuccessful evaluations.
(+>) :: Term AlexPosn -> Term AlexPosn -> Term AlexPosn
(+>) lhs rhs = either error id val where val = interpret (addRight lhs rhs)

-- Add two terms using the left addition combinator
addLeft :: Term AlexPosn -> Term AlexPosn -> Term AlexPosn
addLeft lhs rhs = apply (YComb info addLeftTerm) [lhs, rhs]

-- Add two terms using the right addition combinator
addRight :: Term AlexPosn -> Term AlexPosn -> Term AlexPosn
addRight lhs rhs = apply (YComb info addRightTerm) [lhs, rhs]

-- Combinator for left addition.
--   Recurses on its first argument until it is 0.
addLeftTerm :: Term AlexPosn
addLeftTerm =
  Lambda info "f" (Base :-> Base :-> Base) $
    Lambda info "x" Base $
      Lambda info "y" Base $
        If0
          info
          (Variable info "x")
          (Variable info "y")
          ( Succ info $
              Apply
                info
                ( Apply
                    info
                    (Variable info "f")
                    (Pred info (Variable info "x"))
                )
                (Variable info "y")
          )

-- Combinator for right addition.
--   Recurses on its second argument until it is 0.
addRightTerm :: Term AlexPosn
addRightTerm =
  Lambda info "f" (Base :-> Base :-> Base) $
    Lambda info "x" Base $
      Lambda info "y" Base $
        If0
          info
          (Variable info "y")
          (Variable info "x")
          ( Succ info $
              Apply
                info
                ( Apply
                    info
                    (Variable info "f")
                    (Pred info (Variable info "y"))
                )
                (Variable info "x")
          )
