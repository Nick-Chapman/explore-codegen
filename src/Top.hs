module Top (main) where

import Data.Word8 (Word8)
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
  putStrLn "*explore-codegen*"
  let prog = prog3
  print ("prog",prog)
  let semH = semProg prog
  print ("semH",semH)
  let CompRes{code,alloc} = compile theConfig prog
  print ("alloc",alloc)
  print ("code",code)
  let Prog{signature} = prog
  let semL = semCode code signature alloc
  print ("semL",semL)
  print ("semantics preserved", semH == semL)
  pure ()

_prog0 :: Prog
_prog0 = Prog
  { signature = Signature { inputs = []
                          , outputs = []
                          }
  , body = Stat0
  }

_prog1 :: Prog
_prog1 = Prog
  { signature = Signature { inputs = []
                          , outputs = [v1]
                          }
  , body = Bind { lhs = v1, rhs = Lit8 42, body = Stat0 }
  }
  where v1 = Var "v1"

_prog2 :: Prog
_prog2 = Prog
  { signature = Signature { inputs = [x]
                          , outputs = [q]
                          }
  , body =
      Bind { lhs = v1, rhs = Lit8 42, body =
               Bind { lhs = q, rhs = Add v1 x, body =
                        Stat0 } }
  }
  where
    v1 = Var "v1"
    x = Var "x"
    q = Var "q"


prog3 :: Prog
prog3 = Prog
  { signature = Signature { inputs = [x,y]
                          , outputs = [q]
                          }
  , body =
    Bind { lhs = v1, rhs = Lit8 1, body =
             Bind { lhs = v2, rhs = Add x v1, body =
                      Bind { lhs = q, rhs = Add v2 y, body =
                               Stat0 }}}}
  where
    v1 = Var "v1"
    v2 = Var "v2"
    x = Var "x"
    y = Var "y"
    q = Var "q"


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
-- high level code
data Prog = Prog { signature :: Signature, body :: Stat }
  deriving Show

data Stat where
  Bind :: { lhs :: Var, rhs :: Exp, body :: Stat } -> Stat
  --Unbind :: { var :: Var, body :: Stat } -> Stat
  Stat0 :: Stat
  deriving Show

data Exp
  = Add Var Var -- 8 bit addition, with no carry in or out
--  | ShiftL Var
  | Lit8 Byte
  deriving Show

----------------------------------------------------------------------
-- The act of compiling returns the code & a alloc chosen for I/O
-- TODO: implement compilation such that the semanics are preserved

data CompRes = CompRes { code :: [Op], alloc :: Alloc }

compile :: CompileConfig -> Prog -> CompRes
compile config prog  = CompRes { code, alloc }
  where
    CompileConfig{argRegs,resRegs,temps} = config
    Prog{signature,body} = prog
    Signature{inputs=iVars,outputs=oVars} = signature

    alloc = initAlloc (zip iVars iRegs ++ zip oVars oRegs)
      where
        iRegs = takeStrict (length iVars) argRegs
        oRegs = takeStrict (length oVars) resRegs

    -- clear Acc before main body of code
    varInA = the [ x | x <- iVars, lookAlloc x alloc == RegA ]
    nA = firstUnusedRegZP alloc
    codeToClearA = move RegA (RegZP nA)
    alloc1 = extendAlloc varInA (RegZP nA) alloc

    code :: [Op]
    code = codeToClearA ++ compileStat alloc1 body

    firstUnusedRegZP :: Alloc -> Byte
    firstUnusedRegZP alloc =
      head [ n | r@(RegZP n) <- temps, not (isAllocated r alloc) ]

    compileStat :: Alloc -> Stat -> [Op]
    compileStat alloc = \case
      Stat0 -> []
      Bind{lhs=var,rhs,body} -> do
        let code1 = compileExp alloc rhs
        let n = firstUnusedRegZP alloc
        let reg = RegZP n
        let alloc' = extendAlloc var reg alloc
        let code2 = compileStat alloc' body
        code1 ++ [STA n] ++ code2

    compileExp :: Alloc -> Exp -> [Op] -- into accumulator
    compileExp alloc = \case
      Add v1 v2 -> do
        let n = firstUnusedRegZP alloc
        let reg1 = lookAlloc v1 alloc
        let reg2 = lookAlloc v2 alloc
        move reg2 (RegZP n) ++ move reg1 RegA ++ [CLC,ADCz n]

--      ShiftL{} -> undefined
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
-- low level code
--data Mode = Mode_Immediate | Mode_ZP
data Op where
  LDAi :: Byte -> Op
  LDAz :: Byte -> Op
  STA :: Byte -> Op
  STY :: Byte -> Op
  CLC :: Op
  ADCz :: Byte -> Op
  TYA :: Op

  deriving Show

----------------------------------------------------------------------
-- register allocation, chosen during compilation

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
--      ShiftL{} -> undefined
      Lit8 byte -> Const8 byte


    restrictSem :: [Var] -> Sem -> Sem
    restrictSem xs sem = initSem [ (x,lookSem x sem) | x <- xs ]

----------------------------------------------------------------------
-- Signature

data Signature = Signature { inputs :: [Var], outputs :: [Var] }
  deriving Show

-- high level var names
newtype Var = Var { unVar :: String }
  deriving (Eq,Ord,Show)

----------------------------------------------------------------------
-- semantics of high level code

semCode :: [Op] -> Signature -> Alloc -> Sem
semCode code signature alloc = sem
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
    envFinal = loop env0 code

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


----------------------------------------------------------------------
-- semantics...

data Sem = Sem { m :: Map Var Sv8 }
  deriving (Eq,Show)

initSem :: [(Var,Sv8)] -> Sem
initSem xs = Sem $ Map.fromList xs

lookSem :: Var -> Sem -> Sv8
lookSem x Sem{m} =
  maybe (error (show ("lookSem",show x))) id $ Map.lookup x m

extendSem :: Var -> Sv8 -> Sem -> Sem
extendSem x v Sem{m} = Sem $ Map.insert x v m

data Sv8
  = Const8 Byte
  | Symbolic8 Sym8
  | Add8 Sv8 Sv8 Sv1
--  | ShiftL8 Sv8
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

----------------------------------------------------------------------

newtype Byte = Byte Word8 -- 8bit literal value (0..255)
  deriving (Eq,Ord,Show,Enum,Num)

-- runtime location for an 8 bit values
data Reg = RegA | RegY | RegZP Byte -- RegX
  deriving (Eq,Ord,Show)

-- runtime location for an 1 bit values
data Flag = FlagC
  deriving Eq

----------------------------------------------------------------------
-- misc

takeStrict :: Int -> [a] -> [a] -- crash if not enough
takeStrict n xs =
  if n > length xs then error "takeStrict" else take n xs


the :: [a] -> a -- like head, but expected a list of exactly size 1
the = \case
  [] -> error "the[]"
  [x] -> x
  _ -> error "the>=2"
