-- Low-level 6502 assembly code.
module Asm
  ( Op(..)
  ) where

import Semantics (Byte)

-- data Mode = Mode_Immediate | Mode_ZP

data Op where
  LDAi :: Byte -> Op
  LDAz :: Byte -> Op
  STA :: Byte -> Op
  STY :: Byte -> Op
  CLC :: Op
  ADCz :: Byte -> Op
  TYA :: Op
  ASL :: Op

  deriving Show
