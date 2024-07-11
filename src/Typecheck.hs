{-# LANGUAGE LambdaCase #-}

module Typecheck where

import Common
import Control.Monad
import Evaluation
import Primitive
import RawSyntax
import Syntax
import Text.Megaparsec
import Text.Printf (printf)
import Values

type Types = [(Name, VTy)]

data Cxt = Cxt {env :: Env, types :: Types, lvl :: Lvl, pos :: SourcePos}

bind :: Name -> VTy -> Cxt -> Cxt
bind x a (Cxt env types l pos) =
  Cxt (VVar l : env) ((x, a) : types) (l + 1) pos

define :: Name -> Val -> VTy -> Cxt -> Cxt
define x t a (Cxt env types l pos) =
  Cxt (t : env) ((x, a) : types) (l + 1) pos

emptyCxt :: SourcePos -> Cxt
emptyCxt = Cxt [] [] 0

type TypeError = Either (String, SourcePos)

report :: Cxt -> String -> TypeError a
report cxt msg = Left (msg, pos cxt)

showVal :: Cxt -> Val -> String
showVal cxt v = prettyTm 0 (map fst (types cxt)) (quote (lvl cxt) v) []

check :: Cxt -> RTm -> VTy -> TypeError Tm
check cxt t a =
  case (t, a) of
    (RSrcPos pos t, a) -> check (cxt {pos = pos}) t a
    (RLam x t, VPi x' a b) -> Lam x <$> check (bind x a cxt) t (b $$ VVar (lvl cxt))
    (RLet x a t u, a') -> do
      a <- check cxt a VType
      let va = eval (env cxt) a
      t <- check cxt t va
      let vt = eval (env cxt) t
      u <- check (define x vt va cxt) u a'
      pure (Let x a t u)
    (RLit (I i), VLit (PrimTy IntType)) -> pure $ Lit $ I i
    (RLit (D d), VLit (PrimTy DoubleType)) -> pure $ Lit $ D d
    (RLit (PrimTy IntType), VType) -> pure $ Lit $ PrimTy IntType
    _ -> do
      (t', tty) <- infer cxt t
      unless (conv (lvl cxt) tty a) $
        report
          cxt
          ( printf
              "Type mismatch\n\nexpected type:\n\n  %s\n\ninferred type:\n\n  %s\n"
              (showVal cxt a)
              (showVal cxt tty)
          )
      pure t'

infer :: Cxt -> RTm -> TypeError (Tm, VTy)
infer cxt = \case
  RSrcPos pos t -> infer (cxt {pos = pos}) t
  RVar x -> do
    let go _ [] = report cxt ("Variable out of scope " ++ x)
        go i ((x', a) : tys)
          | x == x' = pure (Var i, a)
          | otherwise = go (i + 1) tys
    go 0 (types cxt)
  RType -> pure (Type, VType)
  RLit (PrimTy a) -> pure (Lit $ PrimTy a, VType)
  RLit (I i) -> pure (Lit $ I i, VLit $ PrimTy IntType)
  RLit (D d) -> pure (Lit $ D d, VLit $ PrimTy DoubleType)
  ROp o -> pure (Op o, VPi "_" (VLit $ PrimTy IntType) (Closure (env cxt) (Pi "_" (Lit $ PrimTy IntType) (Lit $ PrimTy IntType))))
  RBinOpPat o e1 e2 -> do
    (e1', ty1) <- infer cxt e1
    (e2', ty2) <- infer cxt e2
    case (ty1, ty2) of
      (VLit (PrimTy DoubleType), VLit (PrimTy DoubleType)) -> pure (nf (env cxt) (BinOpPat o e1' e2'), VLit $ PrimTy DoubleType)
      (VLit (PrimTy IntType), VLit (PrimTy IntType)) -> pure (nf (env cxt) (BinOpPat o e1' e2'), VLit $ PrimTy IntType)
      _ -> report cxt (printf "Type mismatch\n\nexpected type:\n\n  %s\n\ninferred type:\n\n  %s\n" (showVal cxt ty1) (showVal cxt ty2))
  RApp t u -> do
    (t, tty) <- infer cxt t
    case tty of
      VPi _ a b -> do
        u <- check cxt u a
        pure (App t u, b $$ eval (env cxt) u)
      tty ->
        report cxt $ "Expected a function type, instead inferred:\n\n " ++ showVal cxt tty
  RLam _ _ -> report cxt "Can't infer type for lmbda expression"
  RPi x a b -> do
    a <- check cxt a VType
    b <- check (bind x (eval (env cxt) a) cxt) b VType
    pure (Pi x a b, VType)
  RLet x a t u -> do
    a <- check cxt a VType
    let va = eval (env cxt) a
    t <- check cxt t va
    let vt = eval (env cxt) t
    (u, uty) <- infer (define x vt va cxt) u
    pure (Let x a t u, uty)
