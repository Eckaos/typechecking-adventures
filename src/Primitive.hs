{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Primitive where

data PrimType
  = IntType
  | NatType
  | UnitType
  | DoubleType
  deriving (Eq)

data Constant
  = I Int
  | N Int
  | D Double
  | Unit
  | PrimTy PrimType
  deriving (Eq)

data PrimOp = Add | Mul | Sub | Div

instance Show PrimType where
  show IntType = "Int"
  show DoubleType = "Double"
  show NatType = "Nat"
  show UnitType = "Unit"

instance Show Constant where
  show (I i) = show i
  show (N n) = show n
  show (D d) = show d
  show (PrimTy t) = show t
  show Unit = "()"

instance Show PrimOp where
  show Add = "+"
  show Mul = "*"
  show Sub = "-"
  show Div = "/"
