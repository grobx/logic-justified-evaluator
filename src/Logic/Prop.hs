module Logic.Prop
  ( Prop (..)
  ) where

import Data.Bool

data Prop v = Const Bool
            | Var v
            | Not (Prop v)
            | And (Prop v) (Prop v)
            | Or (Prop v) (Prop v)
            | Imply (Prop v) (Prop v)
            | DoubleImply (Prop v) (Prop v)

instance Functor Prop where
  fmap f = g where
    g (Const v) = Const v
    g (Var v) = Var (f v)
    g (Not p) = Not (g p)
    g (And lhp rhp) = And (g lhp) (g rhp)
    g (Or lhp rhp) = Or (g lhp) (g rhp)
    g (Imply lhp rhp) = Imply (g lhp) (g rhp)
    g (DoubleImply lhp rhp) = DoubleImply (g lhp) (g rhp)
