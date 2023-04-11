module Value (Env, Closure, Value (..), Stuck (..)) where

import Syntax (Term)

type Env = [Value]

type Closure = (Term, Env)

data Value
  = Pi Value Closure -- base, family
  | Sg Value Closure -- base, family
  | Lam Closure
  | Bool
  | True
  | False
  | Pair Value Value
  | Stuck Stuck Value -- second argument is the type
  deriving (Show, Eq)

data Stuck
  = Var Int
  | Fst Stuck
  | Snd Stuck
  | App Stuck Value Value -- fn, arg, base
  | BoolInd Closure Value Value Stuck -- motive, tcase, fcase, scrut
  deriving (Show, Eq)