{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module RawSyntax where

import qualified Common as C
import Primitive
import Text.Megaparsec.Pos

data RTm
  = RVar C.Name
  | RType
  | RLam C.Name RTm
  | RLet C.Name RTm RTm RTm
  | RApp RTm RTm
  | RPi C.Name RTm RTm
  | RLit Constant
  | ROp PrimOp
  | RAnn RTm RTm
  | RSrcPos SourcePos RTm

pattern RBinOpPat :: PrimOp -> RTm -> RTm -> RTm
pattern RBinOpPat o e1 e2 = RApp (RApp (ROp o) e1) e2

pattern RUnOpPat :: PrimOp -> RTm -> RTm
pattern RUnOpPat o e = RApp (ROp o) e

instance Show RTm where
  show (RVar n) = n
  show RType = "Type"
  show (RLam n t) = "\\" ++ n ++ " . " ++ show t
  show (RLet n t u v) = "let " ++ n ++ " : " ++ show t ++ " = " ++ show u ++ " in " ++ show v
  show (RApp t u) = "((" ++ show t ++ ") " ++ show u ++ ")"
  show (RPi n t u) = "(" ++ n ++ " : " ++ show t ++ ") -> " ++ show u
  show (RLit c) = show c
  show (ROp o) = show o
  show (RAnn a b) = "(" ++ show a ++ " : " ++ show b ++ ")"
  show (RSrcPos s t) = "(" ++ show s ++ "," ++ show t ++ ")"
