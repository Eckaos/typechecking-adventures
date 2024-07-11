{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

module Values where

import Common
import Primitive
import Syntax

type Env = [Val]

data Closure = Closure Env Tm deriving (Show)

type VTy = Val

data Val
  = VVar Lvl
  | VType
  | VLam Name Closure
  | VApp Val Val
  | VPair Val Val
  | VPi Name VTy Closure
  | VLit Constant
  | VOp PrimOp
  deriving (Show)

pattern VBinOpPat :: PrimOp -> Val -> Val -> Val
pattern VBinOpPat o e1 e2 = VApp (VApp (VOp o) e1) e2

pattern VUnOpPat :: PrimOp -> Val -> Val
pattern VUnOpPat o e = VApp (VOp o) e
