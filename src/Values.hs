{-# LANGUAGE GADTs #-}

module Values where

import Common
import Primitive
import Syntax

type Env = [Val]

data Closure = Closure Env Tm

data DClosure = DClosure Env Branch

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
