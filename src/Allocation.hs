-- Register allocation, chosen during compilation
module Allocation
  ( Alloc
  , initAlloc
  , lookAlloc
  , extendAlloc
  , isAllocated
  , Reg(..)
  ) where

import Data.Map (Map)
import Semantics (Var,Byte)
import qualified Data.Map as Map

data Alloc = Alloc { m :: Map Var Reg}
  deriving (Eq,Show)

initAlloc :: [(Var,Reg)] -> Alloc
initAlloc xs = Alloc $ Map.fromList xs

lookAlloc :: Var -> Alloc -> Reg
lookAlloc x Alloc{m} =
  maybe (error (show ("lookAlloc",show x))) id $ Map.lookup x m

extendAlloc :: Var -> Reg -> Alloc -> Alloc
extendAlloc x v Alloc{m} = Alloc $ Map.insert x v m

isAllocated :: Reg -> Alloc -> Bool
isAllocated r Alloc{m} = r `elem` (Map.elems m) -- TODO: improve efficiency!


-- Runtime location for an 8 bit values
data Reg = RegA | RegY | RegZP Byte -- RegX
  deriving (Eq,Ord,Show)

-- Runtime location for an 1 bit values
data Flag = FlagC
  deriving Eq
