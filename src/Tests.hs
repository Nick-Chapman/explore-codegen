-- Test compilation preserves semantics.
module Tests (run) where

import Compilation (CompRes(..),theConfig,compile)
import Meaning (semProg,semCode)
import Program (Prog(..),Signature(..),Stat(..),Exp(..))
import Semantics (Var(..))

-- TODO: allow easier construction of test examples
run :: IO ()
run = do
  run1 prog0
  run1 prog1
  run1 prog2
  run1 prog3
  run1 prog4
  run1 prog5
  pure ()

prog5 :: Prog
prog5 = Prog
  { signature = Signature { inputs = [x,y]
                          , outputs = [q]
                          }
  , body = Bind { lhs = q, rhs = ShiftL y, body = Stat0 }
  }
  where
    x = Var "x"
    y = Var "y"
    q = Var "q"


prog4 :: Prog
prog4 = Prog
  { signature = Signature { inputs = [x]
                          , outputs = [q]
                          }
  , body = Bind { lhs = q, rhs = ShiftL x, body = Stat0 }
  }
  where
    x = Var "x"
    q = Var "q"

prog0 :: Prog
prog0 = Prog
  { signature = Signature { inputs = []
                          , outputs = []
                          }
  , body = Stat0
  }

prog1 :: Prog
prog1 = Prog
  { signature = Signature { inputs = []
                          , outputs = [v1]
                          }
  , body = Bind { lhs = v1, rhs = Lit8 42, body = Stat0 }
  }
  where v1 = Var "v1"

prog2 :: Prog
prog2 = Prog
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


-- TODO: use a small testing framework
-- TODO: test different compilation configs -- paramater/result passing ABI
run1 :: Prog -> IO ()
run1 prog = do
  let CompRes{code,alloc} = compile theConfig prog
  let Prog{signature} = prog
  let semH = semProg prog
  let semL = semCode code signature alloc
  let check = (semH == semL)
  --print prog
  --print code
  if check then putStrLn "PASS" else do
    print ("prog",prog)
    print ("alloc",alloc)
    print ("code",code)
    print ("semH",semH)
    print ("semL",semL)
    print ("semantics preserved", check)
  pure ()
