{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

module RawSyntax where

import Common
import Primitive
import Text.Megaparsec.Pos

data RTm
  = RVar Name
  | RType
  | RLam Name RTm
  | RLet Name RTm RTm RTm
  | RApp RTm RTm
  | RPi Name RTm RTm
  | RLit Constant
  | ROp PrimOp
  | RSrcPos SourcePos RTm

pattern RAddOp :: RTm -> RTm -> RTm
pattern RAddOp e1 e2 = RApp (RApp (ROp Add) e1) e2

pattern RMulOp :: RTm -> RTm -> RTm
pattern RMulOp e1 e2 = RApp (RApp (ROp Mul) e1) e2

instance Show RTm where
  show (RVar n) = n
  show RType = "Type"
  show (RLam n t) = "\\" ++ n ++ " . " ++ show t
  show (RLet n t u v) = "let " ++ n ++ " : " ++ show t ++ " = " ++ show u ++ " in " ++ show v
  show (RApp t u) = "((" ++ show t ++ ") " ++ show u ++ ")"
  show (RPi n t u) = "(" ++ n ++ " : " ++ show t ++ ") -> " ++ show u
  show (RLit c) = show c
  show (ROp o) = show o
  show (RSrcPos s t) = "(" ++ show s ++ "," ++ show t ++ ")"
