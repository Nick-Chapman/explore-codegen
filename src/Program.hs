-- High level 'program' code.
module Program
  ( Prog(..)
  , Signature(..)
  , Stat(..)
  , Exp(..)
  ) where

import Semantics (Var,Byte)
import Text.Printf (printf)

data Prog = Prog { signature :: Signature, body :: Stat }

data Signature = Signature { inputs :: [Var], outputs :: [Var] }

data Stat where
  BindVar :: { lhs :: Var, rhs :: Exp, body :: Stat } -> Stat
  --Unbind :: { var :: Var, body :: Stat } -> Stat
  Stat0 :: Stat

data Exp
  = Add Var Var -- 8 bit addition, with no carry in or out
  | ShiftL Var
  | Lit8 Byte


instance Show Prog where
  show Prog{signature = Signature{inputs,outputs}, body} =
    printf "\n  %s <- input\n%s  output: %s\n" (show inputs) (show body) (show outputs)

instance Show Stat where
  show s = unlines [ printf "  %s <- %s" (show lhs) (show rhs) | (lhs,rhs) <- bindsOf s ]
    where
      bindsOf = \case
        Stat0 -> []
        BindVar{lhs,rhs,body} -> (lhs,rhs) : bindsOf body

instance Show Exp where
  show = \case
    Add x1 x2 -> show x1 ++ "+" ++ show x2
    ShiftL x -> show x ++ "<<1"
    Lit8 b -> show b
