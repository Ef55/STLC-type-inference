module NameResolution (resolveTyp, resolveTerm) where

import AutoEnv (Fin (..), LocalName (..))
import AutoEnv.Bind.Local qualified as BindL
import Data.SNat (SNatI)
import Parser qualified as P
import Syntax

resolveTyp :: forall m. (SNatI m) => [(P.Identifier, Fin m)] -> P.Typ -> Maybe (Typ m)
resolveTyp c (P.TVar x) = TVar <$> lookup x c
resolveTyp _ P.Hole = Just Hole
resolveTyp c (P.Arrow l r) = Arrow <$> resolveTyp c l <*> resolveTyp c r
resolveTyp c (P.Forall x t) = do
  let c' = (x, FZ) : map (fmap FS) c
  t' <- resolveTyp c' t
  return $ Forall (BindL.bind (LocalName x) t')
resolveTyp _ P.Bool = Just Bool
resolveTyp _ P.Nat = Just Nat

resolveTerm :: forall m n. (SNatI m, SNatI n) => [(P.Identifier, Fin m)] -> [(P.Identifier, Fin n)] -> P.Term -> Maybe (Term m n)
resolveTerm _ c (P.Var x) = Var <$> lookup x c
resolveTerm cT c (P.Abs typ x t) = do
  let c' = (x, FZ) : map (fmap FS) c
  t' <- resolveTerm cT c' t
  typ' <- resolveTyp cT typ
  return $ Abs typ' (BindL.bind (LocalName x) t')
resolveTerm cT c (P.App f a) = App <$> resolveTerm cT c f <*> resolveTerm cT c a
resolveTerm cT c (P.TAbs x t) = do
  let cT' = (x, FZ) : map (fmap FS) cT
  t' <- resolveTerm cT' c t
  return $ TAbs (BindL.bind (LocalName x) (FlipTerm t'))
resolveTerm cT c (P.TApp t typ) = TApp <$> resolveTerm cT c t <*> resolveTyp cT typ
resolveTerm _ _ (P.CTrue) = return CTrue
resolveTerm _ _ (P.CFalse) = return CFalse
resolveTerm _ _ (P.CZero) = return CZero
resolveTerm cT c (P.Succ t) = Succ <$> resolveTerm cT c t
resolveTerm cT c (P.Pred t) = Pred <$> resolveTerm cT c t
resolveTerm cT c (P.IsZero t) = IsZero <$> resolveTerm cT c t
resolveTerm cT c (P.Ite t t1 t2) = Ite <$> resolveTerm cT c t <*> resolveTerm cT c t1 <*> resolveTerm cT c t2
