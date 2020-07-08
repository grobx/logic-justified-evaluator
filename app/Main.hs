module Main where

import Logic.Prop
import Logic.Evaluator.Justified

import Data.Bool
import Data.Char
import Data.Maybe (fromJust)
import qualified Data.Map as M
import qualified Data.Map.Justified as JM (withMap, member)

evaluatorFor :: Ord k => Prop k -> Env ph k -> Bool
evaluatorFor p en = eval en (fmap (\k -> fromJust $ JM.member k en) p)
  
p1bis :: Prop Char
p1bis = DoubleImply (Var 'a') (Var 'b')

p1 :: Env ph Char -> Bool
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

