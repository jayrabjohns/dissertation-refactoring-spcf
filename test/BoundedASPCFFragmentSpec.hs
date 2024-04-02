module BoundedASPCFFragmentSpec where

import BoundedASPCF
import BoundedSPCF
import Test.HUnit

tests :: Test
tests =
  TestList
    [ injectionToASPCF
    ]

injectionToASPCF :: Test
injectionToASPCF =
  TestLabel "should construct tuple representation correctly" $
    TestCase
      ( do
          let f = Lambda "p" (Base `Cross` Base `Cross` Base) $ Lambda "i" Base $ Case (Variable "i") (Variable "p")
          let p = Product [Numeral 1, Numeral 2, Numeral 3]
          let i = Numeral 1
          let evalF = eval $ Apply (Apply f p) i
          -- let evalInjF = do
          --       injF <- inj f
          --       eval $ Apply (Apply injF p) i
          let evalProjInjF = do
                injF <- inj f
                projInjF <- proj injF
                eval $ Apply (Apply projInjF p) i

          -- return $ Apply (Apply injF p) i
          -- let app2 = Apply (Apply (proj (normalise (inj f))) p) i
          -- _ <- putStrLn $ "f := " ++ show f
          -- _ <- putStrLn $ "Inj := " ++ show (injTerm Base)
          -- _ <- putStrLn $ "Inj f := " ++ show (normalise (inj f))
          -- (Closure _ injF) <- interpretIO (inj f)
          -- _ <- putStrLn $ "E[Inj f] = " ++ show injF
          -- _ <- putStrLn $ "proj f := " ++ show (proj f)
          fResult <- runEvalIO evalF emptyEnv
          projInjResult <- runEvalIO evalProjInjF emptyEnv

          _ <- putStrLn $ "E[f] = " ++ show fResult
          _ <- putStrLn $ "E[proj(inj(f))] = " ++ show projInjResult
          assertEqual
            ( "Term doesn't evaluate to the expected result. Term: "
            -- ++ show term
            )
            fResult
            projInjResult
      )

assertEval :: Term -> Environment -> Term -> Test
assertEval term env expectedTerm =
  TestCase
    ( do
        result <- runEvalIO (eval term) env
        assertEqual
          ( "Term doesn't evaluate to the expected result. Term: "
              ++ show term
              ++ ". Environment: "
              ++ show env
          )
          expectedTerm
          result
    )