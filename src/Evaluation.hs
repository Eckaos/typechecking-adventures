{-# LANGUAGE LambdaCase #-}

module Evaluation where

import Syntax
import Values

infix 8 $$

($$) :: Closure -> Val -> Val
($$) (Closure env t) u = eval (u : env) t

lvl2Ix :: Lvl -> Lvl -> Ix
lvl2Ix (Lvl l) (Lvl x) = Ix (l - x - 1)

eval :: Env -> Tm -> Val
eval e = \case
  Var (Ix x) -> e !! x
  Type -> VType
  Lam x t -> VLam x (Closure e t)
  Let _ _ t u -> eval (eval e t : e) u
  App t u ->
    case (eval e t, eval e u) of
      (VLam _ t', u') -> t' $$ u'
      (t', u') -> VApp t' u'
  Pi x a b -> VPi x (eval e a) (Closure e b)

quote :: Lvl -> Val -> Tm
quote l = \case
  VVar x -> Var (lvl2Ix l x)
  VType -> Type
  VLam x t -> Lam x (quote (l + 1) (t $$ VVar l))
  VApp t u -> App (quote l t) (quote l u)
  VPi x a b -> Pi x (quote l a) (quote (l + 1) (b $$ VVar l))

nf :: Env -> Tm -> Tm
nf e t = quote (Lvl (length e)) (eval e t)

conv :: Lvl -> Val -> Val -> Bool
conv l t u =
  case (t, u) of
    (VType, VType) -> True
    (VPi _ a b, VPi _ a' b') -> conv l a a' && conv (l + 1) (b $$ VVar l) (b' $$ VVar l)
    (VLam _ t, VLam _ t') -> conv (l + 1) (t $$ VVar l) (t' $$ VVar l)
    (VLam _ t, u) -> conv (l + 1) (t $$ VVar l) (VApp u (VVar l))
    (u, VLam _ t) -> conv (l + 1) (VApp u (VVar l)) (t $$ VVar l)
    (VVar x, VVar x') -> x == x'
    (VApp t u, VApp t' u') -> conv l t t' && conv l u u'
    _ -> False
