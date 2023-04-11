module Main (main) where

-- import Lib

-- main :: IO ()
-- main = someFunc

import Equality
import Eval as E
import Syntax as S

main :: IO ()
main = do
  print (E.eval' idfunTrue)
  print (E.eval' idfunTrue)
  print (E.eval' swapTrue)
  print (E.eval' swap)
  print (E.eval' (S.App idfun idfun))
  print (E.eval' (S.App (S.App iffMap S.True) S.True))
  print (E.eval' (S.App (S.App iffMap S.True) S.False))
  print (E.eval' (S.Lam (S.App idfun (S.Var 0))))
  -- Test equality for idfun and unetaidfun
  print
    ( Equality.equateTerm
        1
        (E.eval' (S.Pi S.Bool S.Bool))
        (E.eval' idfun)
        (E.eval' unetaidfun)
    )
  -- Test equality for id and \p -> (fst p, snd p)
  print
    ( Equality.equateTerm
        1
        (E.eval' (S.Pi (S.Sg S.Bool S.Bool) (S.Sg S.Bool S.Bool)))
        (E.eval' idfun)
        (E.eval' (S.Lam (S.Pair (S.Fst (S.Var 0)) (S.Snd (S.Var 0)))))
    )

-- X -> X
idfun :: S.Term
idfun = S.Lam (S.Var 0)

unetaidfun :: S.Term
unetaidfun = S.Lam (S.App idfun (S.Var 0))

-- id True
idfunTrue :: S.Term
idfunTrue = S.App idfun S.True

-- Bool -> Bool
swap :: S.Term
swap = S.Lam (S.BoolInd S.Bool S.False S.True (S.Var 0))

-- swap True
swapTrue :: S.Term
swapTrue = S.App swap S.True

-- Bool -> Bool -> Bool (represent an if-only-if)
iffMap :: S.Term
iffMap = S.Lam (S.BoolInd (S.Pi S.Bool S.Bool) idfun swap (S.Var 0))