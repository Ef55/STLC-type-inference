module Parse (allTests) where

import Lexer qualified
import Parser (Term (..), Typ (..))
import Parser qualified
import Test.Tasty
import Test.Tasty.HUnit

t1 :: TestTree
t1 = testCase "Fully annotated" $ do
  let input = "Λ T. λ s: T -> T. λ z: T. s z"
      term = TAbs "T" (Abs (Arrow (TVar "T") (TVar "T")) "s" (Abs (TVar "T") "z" (App (Var "s") (Var "z"))))
  let parsed = Parser.term $ Lexer.lexer input
  assertEqual "Full" term parsed

t2 :: TestTree
t2 = testCase "Unannotated" $ do
  let input = "λ s: _. λ z: _. s z"
      term =Abs Hole "s" (Abs Hole "z" (App (Var "s") (Var "z")))
  let parsed = Parser.term $ Lexer.lexer input
  assertEqual "Full" term parsed

allTests :: TestTree
allTests =
  testGroup
    "Parser"
    [t1, t2]