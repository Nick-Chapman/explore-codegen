-- Types for semantic values.
module Semantics
  ( Byte
  , Var(..)
  , Sv8(..)
  , Sv1(..)
  , Sym8(..)
  , Sym1(..)
  ) where

import Data.Word8 (Word8)

newtype Byte = Byte Word8 -- 8bit literal value (0..255)
  deriving (Eq,Ord,Enum,Num)

-- high level var names
newtype Var = Var { unVar :: String }
  deriving (Eq,Ord)

-- semantic values
data Sv8
  = Const8 Byte
  | Symbolic8 Sym8
  | Add8 Sv8 Sv8 Sv1
  | ShiftL8 Sv8
  deriving (Eq,Show)

data Sv1
  = Const1 Bool
  | Symbolic1 Sym1
  | CarryOutFromAdd Sv8 Sv8 Sv1
  deriving (Eq,Show)

newtype Sym8 = Sym8 String
  deriving (Eq,Show)

newtype Sym1 = Sym1 String
  deriving (Eq,Show)



instance Show Byte where show (Byte x) = show x

instance Show Var where show (Var x) = x
