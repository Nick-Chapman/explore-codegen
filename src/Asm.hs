-- Low-level 6502 assembly code.
module Asm
  ( Op(..)
  , Code(..)
  ) where

import Semantics (Byte)
import Text.Printf (printf)

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
--  deriving Show

instance Show Op where
  show = \case
    LDAi b -> printf "adc #%s" (show b)
    LDAz b -> printf "adc %s" (show b)
    STA b -> printf "sta %s" (show b)
    STY b -> printf "sty %s" (show b)
    CLC -> "clc"
    ADCz b -> printf "adc %s" (show b)
    TYA -> "tya"
    ASL -> "asl"


newtype Code = Code [Op]
instance Show Code where
  show (Code ops) =
    printf "code:\n%s" $
    unlines [ printf "    %s" (show op) | op <- ops ]
