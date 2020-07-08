module Logic.Evaluator.Justified
  ( Key, Env, eval
  ) where

import Logic.Prop

import Data.Bool
import Data.Char
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Map.Justified as JM

type Key ph k = JM.Key ph k
type Env ph k = JM.Map ph k Bool

eval :: Ord k => Env ph k -> Prop (Key ph k) -> Bool
eval en p = f p where
  f (Const c) = c
  f (Var v) = JM.lookup v en
  f (And lhp rhp) = eval en lhp && eval en rhp
  f (Or lhp rhp) = eval en lhp || eval en rhp
  f (Imply lhp rhp) = not $ eval en lhp || eval en rhp
  f (DoubleImply lhp rhp) = eval en (And (Imply lhp rhp) (Imply rhp lhp))
