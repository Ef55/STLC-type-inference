{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Type (allTests) where

import Lexer qualified
import NameResolution qualified
import Parser qualified
import Printer (pp)
import Test.Tasty
import Test.Tasty.HUnit
import Typer qualified

typeTest :: String -> String -> Maybe String -> TestTree
typeTest name term typ = testCase name $ do
  let (Just term') = NameResolution.resolveTerm [] [] $ Parser.term $ Lexer.lexer term
      result = Typer.tc term'
      typ' = typ >>= NameResolution.resolveTyp [] . (Parser.typ <$> Lexer.lexer)
  assertBool
    ("Type mismatch: " <> show (pp <$> result) <> " /= " <> show (pp <$> typ'))
    (typ' == result)

n0, n1, n2 :: TestTree
n0 = typeTest "BB-Nat 0" "λ s: _. λ z: _. z" (Just "∀Z. ∀S. S -> Z -> Z")
n1 = typeTest "BB-Nat 1" "λ s: _. λ z: _. s z" (Just "∀S. ∀Z. (Z -> S) -> Z -> S")
n2 = typeTest "BB-Nat 2" "λ s: _. λ z: _. s (s z)" (Just "∀S. (S -> S) -> S -> S")

sa :: TestTree
sa = typeTest "Self application" "λ s: _. s s" Nothing

tapl1, tapl2, tapl3 :: TestTree
tapl1 = typeTest "Identity" "λx:_. x" (Just "∀X. X -> X")
tapl2 = typeTest "Chaining" "λz:_. λy:_. z (y True)" (Just "∀X1. ∀X0. (X0 -> X1) -> (Bool -> X0) -> X1")
tapl3 = typeTest "Booleans" "λw:_. if True then False else w False" (Just "(Bool -> Bool) -> Bool")

allTests :: TestTree
allTests =
  testGroup
    "Typer"
    [n0, n1, n2, sa, testGroup "TAPL" [tapl1, tapl2, tapl3]]