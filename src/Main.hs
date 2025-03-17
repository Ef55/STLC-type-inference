module Main (main) where

import Control.Arrow ((>>>))
import Control.Monad ((>=>))
import Lexer qualified
import NameResolution qualified
import Parser qualified
import Printer qualified

pipeline :: String -> Maybe String
pipeline = (Lexer.lexer >>> Parser.statement >>> NameResolution.resolve) >=> (return . Printer.pp)

main :: IO ()
main = do
  putStr "Enter program:\n"
  p <- getLine
  case pipeline p of
    Nothing -> putStr "failed\n"
    Just v -> putStr v >> putStr "\n"
  return ()
