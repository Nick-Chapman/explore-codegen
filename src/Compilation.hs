-- Compilation from high-level 'Program' code to low-level 'Asm' code
module Compilation
  ( CompRes(..)
  , theConfig
  , compile
  ) where

import Allocation (Reg(..),Alloc,initAlloc,lookAlloc,extendAlloc,isAllocated)
import Asm (Op(..),Code(..))
import Program (Prog(..),Signature(..),Stat(..),Exp(..))
import Semantics (Byte)

-- register passing convention perhaps
data CompileConfig = CompileConfig
  { argRegs :: [Reg]
  , resRegs :: [Reg]
  , temps :: [Reg]
  }

theConfig :: CompileConfig
theConfig = CompileConfig { argRegs = order, resRegs = order, temps = order }
  where order = [RegA,RegY] ++ [RegZP n | n <- [0..10]]


----------------------------------------------------------------------
-- The act of compiling returns the code & a alloc chosen for I/O
-- TODO: implement compilation such that the semanics are preserved

data CompRes = CompRes { code :: Code, alloc :: Alloc }

compile :: CompileConfig -> Prog -> CompRes
compile config prog  = CompRes { code, alloc }

  -- This simplistic compiation scheme assumes that the acumulator is always free

  where
    CompileConfig{argRegs,resRegs,temps} = config
    Prog{signature,body} = prog
    Signature{inputs=iVars,outputs=oVars} = signature

    alloc = initAlloc (zip iVars iRegs ++ zip oVars oRegs)
      where
        iRegs = takeStrict (length iVars) argRegs
        oRegs = takeStrict (length oVars) resRegs

    code :: Code
    code = Code (codeToClearA ++ compileStat alloc1 body)

    -- clear Acc before main body of code
    (alloc1,codeToClearA) =
      case [ x | x <- iVars, lookAlloc x alloc == RegA ] of
        _:_:_ -> undefined
        [] -> (alloc,[])
        [varInA] -> (alloc1,codeToClearA)
          where
            nA = firstUnusedRegZP alloc
            codeToClearA = move RegA (RegZP nA)
            alloc1 = extendAlloc varInA (RegZP nA) alloc

    firstUnusedRegZP :: Alloc -> Byte
    firstUnusedRegZP alloc =
      head [ n | r@(RegZP n) <- temps, not (isAllocated r alloc) ]

    compileStat :: Alloc -> Stat -> [Op]
    compileStat alloc = \case
      Stat0 -> []
      BindVar{lhs=var,rhs,body} -> do
        let code1 = compileExp alloc rhs
        let n = firstUnusedRegZP alloc
        let reg = RegZP n
        let alloc' = extendAlloc var reg alloc
        let code2 = compileStat alloc' body
        code1 ++ [STA n] ++ code2

    compileExp :: Alloc -> Exp -> [Op] -- with result left in the accumulator
    compileExp alloc = \case
      Add v1 v2 -> do
        let n = firstUnusedRegZP alloc
        let reg1 = lookAlloc v1 alloc
        let reg2 = lookAlloc v2 alloc
        move reg2 (RegZP n) ++ move reg1 RegA ++ [CLC,ADCz n]

      ShiftL v1 -> do
        let reg1 = lookAlloc v1 alloc
        let bug = False -- introduce a bug here to test the testing framework
        if bug then [ASL] else move reg1 RegA ++ [ASL]

      Lit8 byte ->
        [LDAi byte]


    move :: Reg -> Reg -> [Op]
    move r1 r2 = case (r1,r2) of
      (RegA,RegA) -> []
      (RegA,RegY) -> undefined
      (RegA,RegZP n) -> [STA n]
      (RegY,RegA) -> [TYA]
      (RegY,RegY) -> undefined
      (RegY,RegZP n) -> [STY n]
      (RegZP n,RegA) -> [LDAz n]
      (RegZP{},RegY) -> undefined
      (RegZP{},RegZP{}) -> move r1 RegA ++  move RegA r2


----------------------------------------------------------------------
-- misc

takeStrict :: Int -> [a] -> [a] -- crash if not enough
takeStrict n xs =
  if n > length xs then error "takeStrict" else take n xs
