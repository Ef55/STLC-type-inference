module Printer (Disp (..), pp) where

import AutoEnv (LocalName (..))
import AutoEnv.Bind.Local qualified as BindL
import Control.Monad.Reader (MonadReader (ask, local), Reader, asks, runReader)
import Data.SNat (type (+))
import Data.Type.Nat (Nat (..))
import Data.Vec (Vec, (!))
import Data.Vec qualified as Vec
import Prettyprinter as PP
import Prettyprinter.Render.String (renderString)
import Syntax (FlipTerm (..), Term (..), Typ (..))

data Context m n = Context
  { typBinders :: Vec m LocalName,
    termBinders :: Vec n LocalName,
    level :: Int
  }

newtype Printer m n t = Printer (Reader (Context m n) t)
  deriving (Functor, Applicative, Monad, MonadReader (Context m n))

runPrinter :: Context m n -> Printer m n t -> t
runPrinter ctx (Printer m) = flip runReader ctx $ m

emptyContext :: Context Z Z
emptyContext = Context {typBinders = Vec.empty, termBinders = Vec.empty, level = 0}

addTypBinder :: LocalName -> Printer (S m) n r -> Printer m n r
addTypBinder name = addTypBinders (Vec.singleton name)

addTypBinders :: forall l m n r. Vec l LocalName -> Printer (l + m) n r -> Printer m n r
addTypBinders binders k = do
  ctx :: Context m n <- ask
  let ctx' = ctx {typBinders = Vec.append binders (typBinders ctx)}
  return $ runPrinter ctx' k

addTermBinder :: LocalName -> Printer m (S n) r -> Printer m n r
addTermBinder name = addTermBinders (Vec.singleton name)

addTermBinders :: forall l m n r. Vec l LocalName -> Printer m (l + n) r -> Printer m n r
addTermBinders binders k = do
  ctx :: Context m n <- ask
  let ctx' = ctx {termBinders = Vec.append binders (termBinders ctx)}
  return $ runPrinter ctx' k

atLevel :: (MonadReader (Context m n) c) => Int -> c (Doc ann) -> c (Doc ann)
atLevel l' k = do
  l <- asks level
  r <- local (\ctx -> ctx {level = l'}) k
  return $ if l' < l then PP.parens r else r

class Disp d m n where
  disp :: d -> Printer m n (Doc a)

pp :: (Disp d Z Z) => d -> String
pp =
  renderString
    . layoutPretty defaultLayoutOptions
    . runPrinter emptyContext
    . disp

dispName :: LocalName -> Doc ann
dispName = PP.pretty . name

instance Disp LocalName m n where
  disp (LocalName n) = pure $ PP.pretty n

instance Disp (Typ m) m n where
  disp (TVar i) = atLevel 3 $ do
    binders <- asks typBinders
    return $ PP.pretty $ show $ binders ! i
  disp Hole = atLevel 3 $ return $ PP.pretty "_"
  disp (UVar n) = atLevel 3 $ return $ PP.pretty "_" <> PP.pretty n
  disp (Arrow l r) = atLevel 2 $ do
    dl <- atLevel 3 $ disp l
    dr <- atLevel 2 $ disp r
    return $ dl <+> PP.pretty "->" <+> dr
  disp (Forall bnd) = atLevel 1 $ do
    let (x, t) = BindL.unbind bnd
    dx <- disp x
    dt <- atLevel 1 $ addTypBinder x $ disp t
    return $ PP.pretty "∀" <+> dx <+> PP.pretty "." <+> dt

instance Disp (Term m n) m n where
  disp (Var i) = do
    binders <- asks termBinders
    return $ PP.pretty $ show $ binders ! i
  disp (Abs typ bnd) = do
    let (x, t) = BindL.unbind bnd
    dtyp <- disp typ
    dt <- atLevel 0 $ addTermBinder x $ disp t
    return $
      PP.pretty "λ"
        <+> dispName x
        <+> PP.pretty ":"
        <+> dtyp
        <+> PP.pretty "."
        <+> dt
  disp (App f a) = do
    df <- atLevel 2 $ disp f
    da <- atLevel 3 $ disp a
    return $ df <+> da
  disp (TAbs bnd) = do
    let (x, t) = BindL.unbind bnd
    dt <- atLevel 0 $ addTypBinder x $ disp t
    return $ PP.pretty "Λ" <+> PP.pretty "." <+> dt
  disp (TApp l r) = do
    dl <- atLevel 2 $ disp l
    dr <- atLevel 0 $ disp r
    return $ dl <+> PP.brackets dr

instance Disp (FlipTerm n m) m n where
  disp (FlipTerm t) = disp t