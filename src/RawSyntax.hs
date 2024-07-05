{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

module RawSyntax where

import Common
import Text.Megaparsec.Pos

data RTm
  = RVar Name
  | RType
  | RLam Name RTm
  | RLet Name RTm RTm RTm
  | RApp RTm RTm
  | RPi Name RTm RTm
  | RSrcPos SourcePos RTm

instance Show RTm where
  show (RVar n) = n
  show RType = "Type"
  show (RLam n t) = "\\" ++ n ++ " . " ++ show t
  show (RLet n t u v) = "let " ++ n ++ " : " ++ show t ++ " = " ++ show u ++ " in " ++ show v
  show (RApp t u) = "((" ++ show t ++ ") " ++ show u ++ ")"
  show (RPi n t u) = "(" ++ n ++ " : " ++ show t ++ ") -> " ++ show u
  show (RSrcPos s t) = "(" ++ show s ++ "," ++ show t ++ ")"
