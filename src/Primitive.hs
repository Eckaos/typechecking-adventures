{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Primitive where

data PrimType
  = IntType
  | NatType
  | DoubleType
  deriving (Eq)

data Constant
  = I Int
  | N Int
  | D Double
  | PrimTy PrimType
  deriving (Eq)

data PrimOp = Add | Mul | Sub | Div

instance Show PrimType where
  show IntType = "Int"
  show DoubleType = "Double"
  show NatType = "Nat"

instance Show Constant where
  show (I i) = show i
  show (N n) = show n
  show (D d) = show d
  show (PrimTy t) = show t

instance Show PrimOp where
  show Add = "+"
  show Mul = "*"
  show Sub = "-"
  show Div = "/"
