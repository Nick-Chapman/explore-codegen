-- Test compilation preserves semantics.
module Tests (run) where

import Program (Prog(..),Signature(..),Stat(..),Exp(..))
import Semantics (Var(..))

import Testing (test)
import qualified Testing (run)

run :: IO ()
run = Testing.run $ do
  test prog0
  test prog1
  test prog2
  test prog3
  test prog4
  test prog5

prog5 :: Prog
prog5 = Prog
  { signature = Signature { inputs = [x,y]
                          , outputs = [q]
                          }
  , body = BindVar { lhs = q, rhs = ShiftL y, body = Stat0 }
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
  , body = BindVar { lhs = q, rhs = ShiftL x, body = Stat0 }
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
  , body = BindVar { lhs = v1, rhs = Lit8 42, body = Stat0 }
  }
  where v1 = Var "v1"

prog2 :: Prog
prog2 = Prog
  { signature = Signature { inputs = [x]
                          , outputs = [q]
                          }
  , body =
      BindVar { lhs = v1, rhs = Lit8 42, body =
               BindVar { lhs = q, rhs = Add v1 x, body =
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
    BindVar { lhs = v1, rhs = Lit8 1, body =
             BindVar { lhs = v2, rhs = Add x v1, body =
                      BindVar { lhs = q, rhs = Add v2 y, body =
                               Stat0 }}}}
  where
    v1 = Var "v1"
    v2 = Var "v2"
    x = Var "x"
    y = Var "y"
    q = Var "q"
