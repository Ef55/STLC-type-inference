module Typer (tc) where

import AutoEnv (Fin (..), LocalName (LocalName), Nat (..), shift1E)
import AutoEnv.Bind.Local (bind, getBody, unbind)
import Control.Monad.State
import Data.Fin qualified as Fin
import Data.Function ((&))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.SNat (SNatI, type (+))
import Data.Set qualified as Set
import Data.Vec (Vec, (!))
import Data.Vec qualified as Vec
import Syntax

data Constrain where
  Eq :: Typ Z -> Typ Z -> Constrain

data Context m n = Context
  { -- typBinders :: Vec m LocalName,
    termBinders :: Vec n (LocalName, Typ m),
    freshVar :: Int
  }

newtype TC m n a = TC (State (Context m n) a)
  deriving (Functor, Applicative, Monad, MonadState (Context m n))

emptyContext :: Context Z Z
emptyContext = Context {termBinders = Vec.empty, freshVar = 0}

runTC :: Context m n -> TC m n a -> a
runTC s (TC c) = evalState c s

freshUVar :: TC m n (Typ m)
freshUVar = do
  x <- gets freshVar
  modify (\s -> s {freshVar = x + 1})
  return $ UVar x

addTermBinder :: LocalName -> Typ m -> TC m (S n) r -> TC m n r
addTermBinder n t = addTermBinders (Vec.singleton (n, t))

addTermBinders :: forall l m n r. Vec l (LocalName, Typ m) -> TC m (l + n) r -> TC m n r
addTermBinders binders k = do
  ctx <- get
  let ctx' = ctx {termBinders = Vec.append binders (termBinders ctx)}
  return $ runTC ctx' k

getTyp :: Fin n -> TC m n (Typ m)
getTyp x = do
  binders <- gets termBinders
  return $ snd $ binders ! x

genConstrains :: Term Z n -> TC Z n (Typ Z, [Constrain])
genConstrains (Var x) = (,[]) <$> getTyp x
genConstrains (Abs xT bnd) = do
  let (x, t) = unbind bnd
  xT' <- freshUVar
  (tT, constrains) <- addTermBinder x xT' $ genConstrains t
  return (Arrow xT' tT, Eq xT' xT : constrains)
genConstrains (App l r) = do
  (lT, lConstrains) <- genConstrains l
  (rT, rConstrains) <- genConstrains r
  sT <- freshUVar
  tT <- freshUVar
  let constrains = [Eq lT (Arrow sT tT), Eq rT sT]
  return (tT, constrains <> lConstrains <> rConstrains)
genConstrains (TAbs _) = error "Unsupported"
genConstrains (TApp _ _) = error "Unsupported"

contains :: Int -> Typ m -> Bool
contains _ (TVar _) = False
contains _ Hole = False
contains i (UVar j) = i == j
contains i (Arrow l r) = contains i l || contains i r
contains i (Forall bnd) = getBody bnd & contains i

substU :: forall m. Map.Map Int (Typ m) -> Typ m -> Typ m
substU _ (TVar x) = TVar x
substU _ Hole = Hole
substU r (UVar j) = Map.findWithDefault (UVar j) j r
substU r (Arrow s t) = Arrow (substU r s) (substU r t)
substU r (Forall bnd) =
  let (x, t) = unbind bnd
      r' = fmap (applyE (shift1E @Typ @m)) r
   in substU r' t & bind x & Forall

data Unification1Result where
  Failed1 :: Unification1Result
  Unifier1 :: Map.Map Int (Typ Z) -> Unification1Result
  Constrains1 :: [Constrain] -> Unification1Result

unify1 :: Constrain -> Unification1Result
unify1 c = case c of
  Eq Hole _ -> Unifier1 Map.empty
  Eq _ Hole -> Unifier1 Map.empty
  Eq s t | s == t -> Unifier1 Map.empty
  Eq (UVar x) t | not (contains x t) -> Unifier1 $ Map.singleton x t
  Eq s (UVar x) | not (contains x s) -> Unifier1 $ Map.singleton x s
  Eq (Arrow l r) (Arrow l' r') -> Constrains1 [Eq l l', Eq r r']
  _ -> Failed1

type UnificationResult = Either Constrain (Map.Map Int (Typ Z))

unify :: [Constrain] -> UnificationResult
unify = iter
  where
    iter :: [Constrain] -> UnificationResult
    iter [] = return Map.empty
    iter (c : cs) = case unify1 c of
      Failed1 -> Left c
      Unifier1 u -> do
        let cs' = fmap (\(Eq l r) -> Eq (substU u l) (substU u r)) cs
        u' <- iter cs'
        return $ Map.union u' (fmap (substU u') u)
      Constrains1 cs' -> iter (cs' <> cs)

freshTVar :: (SNatI n) => Fin (S n)
freshTVar = NonEmpty.last Fin.universe1

generalizeOne :: forall m. (SNatI m) => Int -> LocalName -> Typ m -> Typ m
generalizeOne i name = Forall . bind name . iter . applyE @Typ shift1E
  where
    iter :: forall m'. (SNatI m') => Typ (S m') -> Typ (S m')
    iter t =
      case t of
        TVar _ -> t
        Hole -> Hole
        UVar j -> if i == j then TVar freshTVar else t
        Arrow l r -> Arrow (iter l) (iter r)
        Forall bnd ->
          let (x, t'') = unbind bnd
           in Forall $ bind x (iter t'')

collectUVar :: Typ m -> Set.Set Int
collectUVar (TVar _) = Set.empty
collectUVar Hole = Set.empty
collectUVar (UVar i) = Set.singleton i
collectUVar (Arrow l r) = collectUVar l <> collectUVar r
collectUVar (Forall bnd) = collectUVar $ getBody bnd

generalize :: Typ Z -> Typ Z
generalize t = foldl (\t' u -> generalizeOne u (LocalName ("$" <> show u)) t') t (collectUVar t)

tc :: Term Z Z -> Maybe (Typ Z)
tc t =
  let (typ, constrains) = genConstrains t & runTC emptyContext
   in case unify constrains of
        Left _ -> Nothing
        Right u -> Just $ generalize $ substU u typ