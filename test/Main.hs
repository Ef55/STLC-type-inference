module Main (main) where

import Parse qualified
import Test.Tasty
import Type qualified

allTests :: TestTree
allTests =
  testGroup
    "All"
    [ Parse.allTests,
      Type.allTests
    ]

main :: IO ()
main = defaultMain allTests