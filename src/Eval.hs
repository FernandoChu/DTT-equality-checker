module Eval where

import Syntax as S
import Value as V

eval :: V.Env -> S.Term -> V.Value
eval env (S.Var i) = env !! i
eval env (S.Pi base fam) = V.Pi (eval env base) (fam, env)
eval env (S.Sg base fam) = V.Sg (eval env base) (fam, env)
eval env (S.Lam fam) = V.Lam (fam, env)
eval env (S.App fn arg) = app (eval env fn) (eval env arg)
eval env (S.Pair t1 t2) = V.Pair (eval env t1) (eval env t2)
eval env (S.Fst t) = evalFst (eval env t)
eval env (S.Snd t) = evalSnd (eval env t)
eval env S.Bool = V.Bool
eval env S.True = V.True
eval env S.False = V.False
eval env (S.BoolInd m tcase fcase scrut) =
  evalBool (eval env m) (eval env tcase) (eval env fcase) (eval env scrut)

eval' :: S.Term -> V.Value
eval' = eval []

evalFst :: V.Value -> V.Value
evalFst (V.Pair t1 t2) = t1
evalFst (V.Stuck stuck (V.Sg base _)) = V.Stuck (V.Fst stuck) base
evalFst _ = error "can't happen!!"

evalSnd :: V.Value -> V.Value
evalSnd (V.Pair t1 t2) = t2
evalSnd vpair@(V.Stuck stuck (V.Sg base (fam, env))) =
  let u = evalFst vpair
      fiber = eval (u : env) fam
   in V.Stuck (V.Snd stuck) fiber
evalSnd _ = error "can't happen!!"

app :: V.Value -> V.Value -> V.Value
app (V.Lam (term, env)) varg = eval (varg : env) term
app (V.Stuck stuck (V.Pi base (fam, env))) varg =
  let stuck' = V.App stuck varg base
      fiber = eval (varg : env) fam
   in V.Stuck stuck' fiber
app (V.Stuck stuck _) varg = error "can't happen!!"
app _ varg = error "can't happen!!"

evalBool :: V.Value -> V.Value -> V.Value -> V.Value -> V.Value
evalBool m tcase fcase V.True = tcase
evalBool m tcase fcase V.False = fcase
evalBool (V.Pi base (fam, env)) tcase fcase (V.Stuck stuck V.Bool) =
  V.Stuck (V.BoolInd (fam, env) tcase fcase stuck) (V.Pi base (fam, env))
evalBool _ tcase fcase _ = error "evalBool error"