module Logic.Evaluator.Justified
  ( Vname, Key, Env, eval
  ) where

import Logic.Prop

import Data.Bool
import Data.Char
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Map.Justified as JM

type Vname = Char
type Key ph = JM.Key ph Vname
type Env ph = JM.Map ph Vname Bool

eval :: Env ph -> Prop (Key ph) -> Bool
eval _ (Const c) = c
eval en (Var v) = JM.lookup v en
eval en (And lhp rhp) = eval en lhp && eval en rhp
eval en (Or lhp rhp) = eval en lhp || eval en rhp
eval en (Imply lhp rhp) = not $ eval en lhp || eval en rhp
eval en (DoubleImply lhp rhp) = eval en (And (Imply lhp rhp) (Imply rhp lhp))

