{
module Parser (Identifier, term, Term(..), typ, Typ(..)) where
import qualified Lexer
import Lexer (Token)
}

%name term t
%name typ typ

%tokentype {Token}
%token
  let   { Lexer.Let }
  star  { Lexer.Typ }
  var   { Lexer.Var $$ }

  '.'   { Lexer.Dot }
  ','   { Lexer.Comma }
  ':'   { Lexer.Colon }
  '∀'   { Lexer.Forall }
  '->'  { Lexer.Arrow }
  'λ'   { Lexer.Lambda }
  'Λ'   { Lexer.CLambda}
  '_'   { Lexer.Wild }

  '['   { Lexer.Bracket True }
  ']'   { Lexer.Bracket False }
  '('   { Lexer.Parent True }
  ')'   { Lexer.Parent False }

  ':='  { Lexer.Defeq }

  'Bool'    { Lexer.Bool }
  'Nat'     { Lexer.Nat }
  'True'    { Lexer.VTrue }
  'False'   { Lexer.VFalse }
  'Zero'    { Lexer.Zero }
  'Succ'    { Lexer.Succ }
  'Pred'    { Lexer.Pred }
  'IsZero'  { Lexer.IsZero }
  'if'      { Lexer.If }
  'then'    { Lexer.Then }
  'else'    { Lexer.Else }
%%

-- Grammar for types
typ :: { Typ }
typ : typ1 { $1 }

typ1 :: { Typ }
typ1
  : '∀' var '.' typ { Forall $2 $4 }
  | typ2            { $1 }

typ2 :: { Typ }
typ2
  : typ3 '->' typ2   { Arrow $1 $3 }
  | typ3             { $1 }

typ3 :: { Typ }
typ3
  : var           { TVar $1 }
  | '_'           { Hole }
  | '(' typ ')'   { $2 }
  | 'Bool'        { Bool }
  | 'Nat'         { Nat }

-- Grammar for terms
t :: { Term }
t : t1 { $1 }

t1 :: { Term }
t1
  : 'λ' var ':' typ '.' t   { Abs $4 $2 $6 }
  | 'Λ' var '.' t           { TAbs $2 $4 }
  | 'if' t1
    'then' t1
    'else' t1               { Ite $2 $4 $6 }
  | t2                      { $1 }

t2 :: { Term }
t2
  : t2 t3           { App $1 $2 }
  | 'Succ' t3       { Succ $2 }
  | 'Pred' t3       { Pred $2 }
  | 'IsZero' t3     { IsZero $2 }
  | 'Succ' t3       { Succ $2 }
  | t2 '[' typ ']'  { TApp $1 $3 }
  | t3 { $1 }

t3 :: { Term }
t3
  : var         { Var $1 }
  | '(' t ')'   { $2 }
  | 'True'      { CTrue }
  | 'False'     { CFalse }
  | 'Zero'      { CZero }

{

type Identifier = String

data Typ where
  TVar :: Identifier -> Typ
  Hole :: Typ
  Arrow :: Typ -> Typ -> Typ
  Forall :: Identifier -> Typ -> Typ
  Bool :: Typ
  Nat :: Typ
  deriving (Eq, Show)

data Term where
  Var :: Identifier -> Term
  Abs :: Typ -> Identifier -> Term -> Term
  App :: Term -> Term -> Term
  TAbs :: Identifier -> Term -> Term
  TApp :: Term -> Typ -> Term
  CTrue :: Term
  CFalse :: Term
  CZero :: Term
  Succ :: Term -> Term
  Pred :: Term -> Term
  IsZero :: Term -> Term
  Ite :: Term -> Term -> Term -> Term
  deriving (Eq, Show)

happyError :: [Token] -> a
happyError _ = error "Parse error\n"

}