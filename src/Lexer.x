{
module Lexer (lexer, Token(..)) where
}

%wrapper "basic"
%encoding "utf-8"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  -- Whitespace (ignored)
  $white+                         ;
  -- Comments (ignored)
  "--".*                          ;
  -- Keywords
  "let"                           { const Let }
  "Type"                          { const Typ }
  "Bool"                          { const Bool }
  "Nat"                           { const Nat }
  "True"                          { const VTrue }
  "False"                         { const VFalse }
  "0"                             { const Zero}
  "Succ"                          { const Succ }
  "Pred"                          { const Pred }
  "IsZero"                        { const IsZero }
  "if"                            { const If }
  "then"                          { const Then }
  "else"                          { const Else }
  -- Variables
  $alpha [$alpha $digit \_ \']*   { \s -> Var s }
  -- All special characters / operators
  "."                             { const Dot }
  ","                             { const Comma }
  ":"                             { const Colon }
  "\/" | "∀"                      { const Forall }
  "->"                            { const Arrow }
  "\" | "λ"                       { const Lambda }
  "/\" | "Λ"                      { const CLambda }
  "\["                            { const $ Bracket True }
  "\]"                            { const $ Bracket False }
  "("                             { const $ Parent True}
  ")"                             { const $ Parent False}
  ":="                            { const Defeq}
  "_"                             { const Wild }
{

data Token
  = Let
  | Typ
  | Var String
  | Dot
  | Comma
  | Colon
  | Forall
  | Arrow
  | Lambda
  | CLambda
  | Bracket Bool
  | Parent Bool
  | Defeq
  | Wild
  | Nat | Zero | Succ
  | Pred | IsZero
  | Bool | VTrue | VFalse
  | If | Then | Else
  deriving (Eq, Show)

lexer :: String -> [Token]
lexer = alexScanTokens

}