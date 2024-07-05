module Values where

import Common
import Syntax

type Env = [Val]

data Closure = Closure Env Tm

type VTy = Val

data Val
  = VVar Lvl
  | VType
  | VLam Name Closure
  | VApp Val Val
  | VPi Name VTy Closure
