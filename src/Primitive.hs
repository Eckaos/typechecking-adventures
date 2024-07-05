{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Primitive where

data PrimType
  = IntType
  deriving (Eq)

data Constant
  = I Int
  | PrimTy PrimType
  deriving (Eq)

data PrimOp where
  Add, Mul :: PrimOp

instance Show PrimType where
  show IntType = "Integer"

instance Show Constant where
  show (I i) = show i
  show (PrimTy t) = show t

instance Show PrimOp where
  show Add = "+"
  show Mul = "*"
