{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Syntax where

import Common

newtype Ix = Ix Int deriving (Show, Eq, Num) via Int

newtype Lvl = Lvl Int deriving (Show, Eq, Num) via Int

type Branch = [(Name, Tm)]

data Tm
  = Var Ix
  | Type
  | Lam Name Tm
  | Let Name Tm Tm Tm
  | App Tm Tm
  | Pi Name Tm Tm

type Ty = Tm

-- Printing
fresh :: [Name] -> Name -> Name
fresh _ "_" = "_"
fresh ns x
  | x `elem` ns = fresh ns (x ++ "'")
  | otherwise = x

-- Printing precedence
atomp :: Int
atomp = 3 -- Type, var, constant

appp :: Int
appp = 2 -- application

pip :: Int
pip = 1 -- pi

letp :: Int
letp = 0 -- let, lambda

-- Wrap in parenthesis
par :: Int -> Int -> ShowS -> ShowS
par p p' = showParen (p' < p)

prettyTm :: Int -> [Name] -> Tm -> ShowS
prettyTm = go
  where
    piBind ns x a =
      showParen True ((x ++) . (" : " ++) . go letp ns a)
    go :: Int -> [Name] -> Tm -> ShowS
    go p ns = \case
      Var (Ix x) -> ((ns !! x) ++)
      App t u -> par p appp $ go appp ns t . (' ' :) . go atomp ns u
      Lam (fresh ns -> x) t -> par p letp $ ("\\ " ++) . (x ++) . goLam (x : ns) t
        where
          goLam ns (Lam (fresh ns -> x) t) =
            (' ' :) . (x ++) . goLam (x : ns) t
          goLam ns t =
            (". " ++) . go letp ns t
      Type -> ("Type" ++)
      Pi "_" a b -> par p pip $ go appp ns a . (" -> " ++) . go pip ("_" : ns) b
      Pi (fresh ns -> x) a b -> par p pip $ piBind ns x a . goPi (x : ns) b
        where
          goPi ns (Pi "_" a b) =
            (" -> " ++) . go appp ns a . (" -> " ++) . go pip ("_" : ns) b
          goPi ns (Pi (fresh ns -> x) a b) =
            piBind ns x a . goPi (x : ns) b
          goPi ns b =
            (" -> " ++) . go pip ns b
      Let (fresh ns -> x) a t u ->
        par p letp $
          ("let " ++)
            . (x ++)
            . (" : " ++)
            . go letp ns a
            . (" = " ++)
            . go letp ns t
            . ("\nin " ++)
            . go letp (x : ns) u
            . ("\n" ++)

-- DCon branch -> ("data : " ++) . goBranch ns branch

instance Show Tm where showsPrec p = prettyTm p []
