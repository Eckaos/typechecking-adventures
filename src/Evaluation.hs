{-# LANGUAGE LambdaCase #-}

module Evaluation where

import Primitive
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
  BinOpPat o e1 e2 ->
    evalBinOp o (eval e e1) (eval e e2)
  UnOpPat o e' ->
    evalUnOp o (eval e e')
  App t u ->
    case (eval e t, eval e u) of
      (VLam _ t', u') -> t' $$ u'
      (t', u') -> VApp t' u'
  Pair x y -> VPair (eval e x) (eval e y)
  Pi x a b -> VPi x (eval e a) (Closure e b)
  Lit c -> VLit c
  Op o -> VOp o

evalBinOp :: PrimOp -> Val -> Val -> Val
evalBinOp o e1 e2 =
  case (o, e1, e2) of
    (Add, VLit (I x), VLit (I y)) -> VLit $ I (x + y)
    (Sub, VLit (I x), VLit (I y)) -> VLit $ I (x - y)
    (Mul, VLit (I x), VLit (I y)) -> VLit $ I (x * y)
    (Div, VLit (I x), VLit (I y)) -> VLit $ I (x `div` y)
    (Add, VLit (D x), VLit (D y)) -> VLit $ D (x + y)
    (Add, VLit (D x), VLit (I y)) -> VLit $ D (x + fromIntegral y)
    (Add, VLit (I x), VLit (D y)) -> VLit $ D (fromIntegral x + y)
    (Sub, VLit (D x), VLit (D y)) -> VLit $ D (x - y)
    (Sub, VLit (D x), VLit (I y)) -> VLit $ D (x - fromIntegral y)
    (Sub, VLit (I x), VLit (D y)) -> VLit $ D (fromIntegral x - y)
    (Mul, VLit (D x), VLit (D y)) -> VLit $ D (x * y)
    (Mul, VLit (D x), VLit (I y)) -> VLit $ D (x * fromIntegral y)
    (Mul, VLit (I x), VLit (D y)) -> VLit $ D (fromIntegral x * y)
    (Div, VLit (D x), VLit (D y)) -> VLit $ D (x / y)
    (Div, VLit (D x), VLit (I y)) -> VLit $ D (x / fromIntegral y)
    (Div, VLit (I x), VLit (D y)) -> VLit $ D (fromIntegral x / y)
    _ -> VApp (VApp (VOp o) e1) e2

evalUnOp :: PrimOp -> Val -> Val
evalUnOp o e =
  case (o, e) of
    (Sub, VLit (I x)) -> VLit $ I (-x)
    _ -> VApp (VOp o) e

quote :: Lvl -> Val -> Tm
quote l = \case
  VVar x -> Var (lvl2Ix l x)
  VType -> Type
  VLam x t -> Lam x (quote (l + 1) (t $$ VVar l))
  VApp t u -> App (quote l t) (quote l u)
  VPair x y -> Pair (quote l x) (quote l y)
  VPi x a b -> Pi x (quote l a) (quote (l + 1) (b $$ VVar l))
  VLit c -> Lit c
  VOp p -> Op p

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
    (VLit a, VLit a') -> a == a'
    _ -> False
