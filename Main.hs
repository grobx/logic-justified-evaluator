module Main where

import Data.Bool
import Data.Char
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Map.Justified as JM

data Prop v = Const Bool
            | Var v
            | Not (Prop v)
            | And (Prop v) (Prop v)
            | Or (Prop v) (Prop v)
            | Imply (Prop v) (Prop v)
            | DoubleImply (Prop v) (Prop v)

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

evaluatorFor :: Prop Vname -> Env ph -> Bool
evaluatorFor p en =
  eval en (f p) where
    f (Const c) = Const c
    f (Var n) = Var (fromJust $ JM.member n en)
    f (And lhp rhp) = And (f lhp) (f rhp)
    f (Or lhp rhp) = Or (f lhp) (f rhp)
    f (Imply lhp rhp) = Imply (f lhp) (f rhp)
    f (DoubleImply lhp rhp) = DoubleImply (f lhp) (f rhp)
  
p1bis :: Prop Vname
p1bis = DoubleImply (Var 'a') (Var 'b')

p1 :: Env ph -> Bool
p1 en =
  let a = Var $ fromJust $ JM.member 'a' en
      b = Var $ fromJust $ JM.member 'b' en
      p = DoubleImply a b
  in eval en p

main :: IO ()
main = do
  let m = M.fromList [('a',True),('b',True)]
  let rp1 = JM.withMap m p1
  let rp1bis = JM.withMap m (evaluatorFor p1bis)
  putStrLn $ show (rp1 == rp1bis)

