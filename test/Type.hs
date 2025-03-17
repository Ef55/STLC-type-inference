{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Type (allTests) where

import Lexer qualified
import NameResolution qualified
import Parser qualified
import Printer (pp)
import Test.Tasty
import Test.Tasty.HUnit
import Typer qualified

-- If no expected type is provided (using `Nothing`), then the typer is expected
-- to fail.
typeTest :: String -> String -> Maybe String -> TestTree
typeTest name term typ = testCase name $ do
  let (Just term') = NameResolution.resolveTerm [] [] $ Parser.term $ Lexer.lexer term
      result = Typer.tc term'
      typ' = typ >>= NameResolution.resolveTyp [] . (Parser.typ <$> Lexer.lexer)
  assertBool
    ("Type mismatch: " <> show (pp <$> result) <> " /= " <> show (pp <$> typ'))
    (typ' == result)

n0, n1, n2, n3, natToNat :: TestTree
n0 = typeTest "BB-Nat 0" "λ s: _. λ z: _. z" (Just "∀Z. ∀S. S -> Z -> Z")
n1 = typeTest "BB-Nat 1" "λ s: _. λ z: _. s z" (Just "∀S. ∀Z. (Z -> S) -> Z -> S")
n2 = typeTest "BB-Nat 2" "λ s: _. λ z: _. s (s z)" (Just "∀S. (S -> S) -> S -> S")
n3 = typeTest "BB-Nat 2" "λ s: _. λ z: _. s (s (s z))" (Just "∀S. (S -> S) -> S -> S")
natToNat = typeTest "BB-fold" "λ x : _ -> _ -> Nat . x (λn:_. Succ n) 0" (Just "((Nat -> Nat) -> Nat -> Nat) -> Nat")

sa :: TestTree
sa = typeTest "Self application" "λ s: _. s s" Nothing

ite1, ite2 :: TestTree
ite1 = typeTest "Ite (positive)" "λn:_. if IsZero n then Succ n else Pred n" (Just "Nat -> Nat")
ite2 = typeTest "Ite (negative)" "λn:_. if IsZero n then Succ n else IsZero n" Nothing

tapl1, tapl2, tapl3 :: TestTree
tapl1 = typeTest "Identity" "λx:_. x" (Just "∀X. X -> X")
tapl2 = typeTest "Chaining" "λz:_. λy:_. z (y True)" (Just "∀X1. ∀X0. (X0 -> X1) -> (Bool -> X0) -> X1")
tapl3 = typeTest "Booleans" "λw:_. if True then False else w False" (Just "(Bool -> Bool) -> Bool")

allTests :: TestTree
allTests =
  testGroup
    "Typer"
    [ testGroup "Böhm-Berarducci Naturals" [n0, n1, n2, n3, natToNat],
      sa,
      testGroup "If-then-else" [ite1, ite2],
      testGroup "TAPL" [tapl1, tapl2, tapl3]
    ]