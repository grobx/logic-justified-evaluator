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

