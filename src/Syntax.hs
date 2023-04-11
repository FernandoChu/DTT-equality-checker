module Syntax (Term (..)) where

data Term
  = Var Int
  | Pi Term Term -- base, family
  | Sg Term Term -- base, family
  | Lam Term
  | App Term Term -- fn, arg
  | Pair Term Term -- fst, snd
  | Fst Term
  | Snd Term
  | Bool
  | True
  | False
  | BoolInd Term Term Term Term -- motive, tcase, fcase, scrut
  deriving (Show, Eq)
