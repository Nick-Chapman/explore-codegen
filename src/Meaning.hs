-- The semantics of high-level and low-level code
module Meaning
  ( Sem
  , semProg
  , semCode
  ) where

import Allocation (Alloc,Reg(..),lookAlloc)
import Asm (Op(..),Code(..))
import Program (Prog(..),Signature(..),Stat(..),Exp(..))
import Semantics (Var(..),Sv8(..),Sv1(..),Sym8(..),Sym1(..))

import Data.Map (Map)
import qualified Data.Map as Map

----------------------------------------------------------------------
-- semantics of high level code

semProg :: Prog -> Sem
semProg prog = sem
  where
    Prog{signature,body} = prog
    Signature{inputs=iVars,outputs=oVars} = signature

    sem = restrictSem oVars (run sem0 body)

    sem0 = initSem
      [ (var,Symbolic8 sym)
      | var <- iVars
      , let sym = Sym8 (unVar var ++ "-initial-value")
      ]

    run :: Sem -> Stat -> Sem
    run sem = \case
      Bind{lhs,rhs,body} -> run (extendSem lhs (eval sem rhs) sem) body
      --Unbind{var,body} -> run (restrictSem var sem) body
      Stat0 -> sem

    eval :: Sem -> Exp -> Sv8
    eval sem = \case
      Add v1 v2 -> Add8 (lookSem v1 sem) (lookSem v2 sem) (Const1 False)
      ShiftL v1 -> ShiftL8 (lookSem v1 sem)
      Lit8 byte -> Const8 byte


    restrictSem :: [Var] -> Sem -> Sem
    restrictSem xs sem = initSem [ (x,lookSem x sem) | x <- xs ]

----------------------------------------------------------------------
-- semantics of low level code

semCode :: Code -> Signature -> Alloc -> Sem
semCode (Code ops) signature alloc = sem
  where
    Signature{inputs=iVars,outputs=oVars} = signature

    sem :: Sem
    sem = initSem
      [ (var,semVal)
      | var <- oVars
      , let reg = lookAlloc var alloc
      , let semVal = lookEnv reg envFinal
      ]

    envFinal :: Env
    envFinal = loop env0 ops

    env0 :: Env
    env0 = initEnv
      [ (reg,Symbolic8 sym)
      | var <- iVars
      , let reg = lookAlloc var alloc
      , let sym = Sym8 (unVar var ++ "-initial-value")
      ]

    loop :: Env -> [Op] -> Env
    loop env = \case
      [] -> env
      op:code -> loop (execOp env op) code

    execOp :: Env -> Op -> Env
    execOp env = \case
      TYA -> extendEnv RegA (lookEnv RegY env) env
      LDAi n -> extendEnv RegA (Const8 n) env
      LDAz n -> extendEnv RegA (lookEnv (RegZP n) env) env
      STA n -> extendEnv (RegZP n) (lookEnv RegA env) env
      STY n -> extendEnv (RegZP n) (lookEnv RegY env) env
      CLC -> clearCf env
      ADCz n ->
        extendEnv RegA
        (Add8 (lookEnv RegA env) (lookEnv (RegZP n) env) (lookCf env))
        env
      ASL ->
        extendEnv RegA (ShiftL8 (lookEnv RegA env)) env

----------------------------------------------------------------------
-- Env: for computing semantics of low-level code

data Env = Env { m :: Map Reg Sv8, cf :: Sv1 }

initEnv :: [(Reg,Sv8)] -> Env
initEnv xs = Env { m = Map.fromList xs, cf = Symbolic1 (Sym1 "cf-initial") }

lookEnv :: Reg -> Env -> Sv8
lookEnv x Env{m} = maybe (error (show ("lookEnv",show x))) id $ Map.lookup x m

extendEnv :: Reg -> Sv8 -> Env -> Env
extendEnv x sv env@Env{m} = env { m = Map.insert x sv m }

clearCf :: Env -> Env
clearCf env = env { cf = Const1 False }

lookCf :: Env -> Sv1
lookCf Env{cf} = cf



data Sem = Sem { m :: Map Var Sv8 }
  deriving (Eq,Show)

initSem :: [(Var,Sv8)] -> Sem
initSem xs = Sem $ Map.fromList xs

lookSem :: Var -> Sem -> Sv8
lookSem x Sem{m} =
  maybe (error (show ("lookSem",show x))) id $ Map.lookup x m

extendSem :: Var -> Sv8 -> Sem -> Sem
extendSem x v Sem{m} = Sem $ Map.insert x v m
