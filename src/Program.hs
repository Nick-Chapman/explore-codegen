-- High level 'program' code.
module Program
  ( Prog(..)
  , Signature(..)
  , Stat(..)
  , Exp(..)
  ) where

import Semantics (Var,Byte)

data Prog = Prog { signature :: Signature, body :: Stat }
  deriving Show

data Signature = Signature { inputs :: [Var], outputs :: [Var] }
  deriving Show

data Stat where
  Bind :: { lhs :: Var, rhs :: Exp, body :: Stat } -> Stat
  --Unbind :: { var :: Var, body :: Stat } -> Stat
  Stat0 :: Stat
  deriving Show

data Exp
  = Add Var Var -- 8 bit addition, with no carry in or out
  | ShiftL Var
  | Lit8 Byte
  deriving Show
