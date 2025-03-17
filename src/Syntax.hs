module Syntax
  ( Typ (..),
    Term (..),
    FlipTerm (..),
    Subst (..),
  )
where

import AutoEnv
import AutoEnv.Bind.Local qualified as BindL
import Data.Function ((&))

--------------------------------------------------------------------------------
--- The core constructs of the calculus: terms & types
--------------------------------------------------------------------------------

data Typ (m :: Nat) where
  TVar :: Fin m -> Typ m
  Hole :: Typ m
  UVar :: Int -> Typ m
  Arrow :: Typ m -> Typ m -> Typ m
  Forall :: BindL.Bind Typ Typ n -> Typ n
  Bool :: Typ m
  Nat :: Typ m

data Term (m :: Nat) (n :: Nat) where
  -- ?1
  Var :: Fin n -> Term m n
  -- \ ?x : T . t
  Abs :: Typ m -> BindL.Bind (Term m) (Term m) n -> Term m n
  -- t1 t2
  App :: Term m n -> Term m n -> Term m n
  -- /\ ?x . t
  TAbs :: BindL.Bind Typ (FlipTerm n) m -> Term m n
  -- t1 [T2]
  TApp :: Term m n -> Typ m -> Term m n
  CTrue :: Term m n
  CFalse :: Term m n
  CZero :: Term m n
  Succ :: Term m n -> Term m n
  Pred :: Term m n -> Term m n
  IsZero :: Term m n -> Term m n
  Ite :: Term m n -> Term m n -> Term m n -> Term m n

newtype FlipTerm n m = FlipTerm {unflipTerm :: Term m n}

--------------------------------------------------------------------------------
--- Core operations for terms & types
--------------------------------------------------------------------------------

-- deriving instance (Generic1 (Term m))

instance (Eq (Typ m)) => Eq (BindL.Bind Typ Typ m) where
  l == r = BindL.getBody l == BindL.getBody r

deriving instance Eq (Typ m)

instance (Eq (Term m n)) => Eq (BindL.Bind (Term m) (Term m) n) where
  l == r = BindL.getBody l == BindL.getBody r

deriving instance Eq (Term m n)

instance (Eq (Term n m)) => Eq (BindL.Bind Typ (FlipTerm n) m) where
  l == r = BindL.getBody l == BindL.getBody r

deriving instance Eq (FlipTerm n m)

instance SubstVar Typ where
  var = TVar

instance SubstVar (Term m) where
  var = Var

instance Subst Typ Typ where
  applyE e (TVar x) = applyEnv e x
  applyE _ Hole = Hole
  applyE _ (UVar x) = UVar x
  applyE e (Arrow l r) = Arrow (applyE e l) (applyE e r)
  applyE e (Forall bnd) = Forall (applyE e bnd)
  applyE _ Bool = Bool
  applyE _ Nat = Nat

substTyp :: Env Typ m m' -> Term m n -> Term m' n
substTyp e t = t & FlipTerm & applyE e & unflipTerm

substTerm :: Env (Term m) n n' -> FlipTerm n m -> FlipTerm n' m
substTerm e t = t & unflipTerm & applyE e & FlipTerm

instance Subst Typ (FlipTerm n) where
  applyE :: Env Typ m m' -> FlipTerm n m -> FlipTerm n m'
  applyE _ (FlipTerm (Var x)) = FlipTerm $ Var x
  applyE e (FlipTerm (Abs typ bnd)) =
    let (x, t) = BindL.unbind bnd
     in FlipTerm $ Abs (applyE e typ) (BindL.bind x (substTyp e t))
  applyE e (FlipTerm (App f a)) = FlipTerm $ App (substTyp e f) (substTyp e a)
  applyE e (FlipTerm (TAbs t)) = FlipTerm $ TAbs $ applyE e t
  applyE e (FlipTerm (TApp t typ)) = FlipTerm $ TApp (substTyp e t) (applyE e typ)
  applyE _ (FlipTerm CTrue) = FlipTerm CTrue
  applyE _ (FlipTerm CFalse) = FlipTerm CFalse
  applyE _ (FlipTerm CZero) = FlipTerm CZero
  applyE e (FlipTerm (Succ t)) = FlipTerm $ Succ $ substTyp e t
  applyE e (FlipTerm (Pred t)) = FlipTerm $ Pred $ substTyp e t
  applyE e (FlipTerm (IsZero t)) = FlipTerm $ IsZero $ substTyp e t
  applyE e (FlipTerm (Ite t t1 t2)) = FlipTerm $ Ite (substTyp e t) (substTyp e t1) (substTyp e t2)

upTyScope :: Env (Term m) n1 n2 -> Env (Term (S m)) n1 n2
upTyScope = transform (substTyp shift1E)

instance Subst (Term m) (Term m) where
  applyE :: Env (Term m) n n' -> Term m n -> Term m n'
  applyE e (Var x) = applyEnv e x
  applyE e (Abs t bnd) = Abs t (applyE e bnd)
  applyE e (App l r) = App (applyE e l) (applyE e r)
  applyE e (TAbs bnd) =
    let (x, t) = BindL.unbind bnd
     in TAbs $ BindL.bind x (substTerm (upTyScope e) t)
  applyE e (TApp l r) = TApp (applyE e l) r
  applyE _ CTrue = CTrue
  applyE _ CFalse = CFalse
  applyE _ CZero = CZero
  applyE e (Succ t) = Succ $ applyE e t
  applyE e (Pred t) = Pred $ applyE e t
  applyE e (IsZero t) = IsZero $ applyE e t
  applyE e (Ite t t1 t2) = Ite (applyE e t) (applyE e t1) (applyE e t2)