{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Primitive where

data PrimType
  = IntType
  | DoubleType
  deriving (Eq)

data Constant
  = I Int
  | D Double
  | PrimTy PrimType
  deriving (Eq)

data PrimOp = Add | Mul | Sub | Div

instance Show PrimType where
  show IntType = "Int"
  show DoubleType = "Double"

instance Show Constant where
  show (I i) = show i
  show (D d) = show d
  show (PrimTy t) = show t

instance Show PrimOp where
  show Add = "+"
  show Mul = "*"
  show Sub = "-"
  show Div = "/"
