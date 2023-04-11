module Equality where

import Eval
import Syntax as S
import Value as V

-- Lenght of the env, and the two values (types) to equate
equateType :: Int -> V.Value -> V.Value -> ()
equateType len (V.Pi base0 (fam0, env0)) (V.Pi base1 (fam1, env1)) =
  let var = V.Stuck (V.Var len) base0
      fiber0 = Eval.eval (var : env0) fam0
      fiber1 = Eval.eval (var : env1) fam1
   in equateType len base0 base1 `seq` equateType (len + 1) fiber0 fiber1
equateType len (V.Pi base0 (fam0, env0)) _ = error "equateType: unequal types"
equateType len (V.Sg base0 (fam0, env0)) (V.Sg base1 (fam1, env1)) =
  let var = V.Stuck (V.Var len) base0
      fiber0 = Eval.eval (var : env0) fam0
      fiber1 = Eval.eval (var : env1) fam1
   in equateType len base0 base1 `seq` equateType (len + 1) fiber0 fiber1
equateType len (V.Sg base0 (fam0, env0)) _ = error "equateType: unequal types"
equateType len V.Bool V.Bool = ()
equateType len V.Bool _ = ()
equateType len _ _ = error "equateType: unequal types or these are not types"

-- Lenght of the env, the type of the values, and the two values to equate
equateTerm :: Int -> V.Value -> V.Value -> V.Value -> ()
equateTerm len (V.Pi base (fam, env)) val0 val1 =
  let var = V.Stuck (V.Var len) base
      result0 = Eval.app val0 var
      result1 = Eval.app val1 var
      fiber = Eval.eval (var : env) fam
   in equateTerm (len + 1) fiber result0 result1
equateTerm len (V.Sg base (fam, env)) val0 val1 =
  let fst0 = Eval.evalFst val0
      fst1 = Eval.evalFst val1
      snd0 = Eval.evalSnd val0
      snd1 = Eval.evalSnd val1
      fiber = Eval.eval (fst1 : env) fam
   in equateTerm len fiber fst0 fst1 `seq` equateTerm len fiber snd0 snd1
equateTerm len _ V.True V.True = ()
equateTerm len _ V.False V.False = ()
equateTerm len _ (V.Stuck stuck0 tp0) (V.Stuck stuck1 tp1) =
  equateType len tp0 tp1 `seq` equateStuck len stuck0 stuck1
equateTerm len _ _ _ = error "equateTerm: unequal terms"

equateStuck :: Int -> V.Stuck -> V.Stuck -> ()
equateStuck len (V.Var lvl0) (V.Var lvl1) =
  if lvl0 == lvl1 then () else error "equateTerm: unequal levels"
equateStuck len (V.Fst stuck0) (V.Fst stuck1) =
  equateStuck len stuck0 stuck1
equateStuck len (V.Snd stuck0) (V.Snd stuck1) =
  equateStuck len stuck0 stuck1
equateStuck len (V.App fn0 arg0 base0) (V.App fn1 arg1 base1) =
  equateStuck len fn0 fn1 `seq` equateType len base0 base1 `seq` equateTerm len base0 arg0 arg1
equateStuck len _ _ = error "equateStuck: unequal terms"