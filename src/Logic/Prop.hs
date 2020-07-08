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
  fmap f (Const v) = Const v
  fmap f (Var v) = Var (f v)
  fmap f (Not p) = Not (fmap f p)
  fmap f (And lhp rhp) = And (fmap f lhp) (fmap f rhp)
  fmap f (Or lhp rhp) = Or (fmap f lhp) (fmap f rhp)
  fmap f (Imply lhp rhp) = Imply (fmap f lhp) (fmap f rhp)
  fmap f (DoubleImply lhp rhp) = DoubleImply (fmap f lhp) (fmap f rhp)
