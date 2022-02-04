-- | Small testing framework for writng 'sham' regression tests.
module Testing (test,run) where

import Control.Monad (ap,liftM)
import Program (Prog(..))
import Compilation (CompRes(..),theConfig,compile)
import Meaning (semProg,semCode)

-- | construct a single testcase
test :: Prog -> Testing ()
test prog = T1 (Test {prog})

data Test = Test { prog :: Prog }

runTest :: Int -> Test -> IO Bool
runTest n Test{prog} = do
  --putStrLn $ "running test #" ++ show n
  let CompRes{code,alloc} = compile theConfig prog
  --print prog
  --print code
  let Prog{signature} = prog
  let semH = semProg prog
  let semL = semCode code signature alloc
  let check = (semH == semL)
  if check then pure True else do
    putStrLn $ "test #" ++ show n ++ " failed."
    print ("prog",prog)
    print ("alloc",alloc)
    print ("code",code)
    print ("semH",semH)
    print ("semL",semL)
    pure False

run :: Testing () -> IO ()
run testing = do
  bools <- sequence [ runTest i x | (i,x) <- zip [1..] (collect testing) ]
  let numTests = length bools
  let numPass = length [ () | res <- bools, res ]
  let numFail = numTests - numPass
  putStrLn $
    show numTests ++ " tests ran; " ++ (if numFail > 0 then show numFail ++ " fail." else "all pass.")

instance Functor Testing where fmap = liftM
instance Applicative Testing where pure = return; (<*>) = ap
instance Monad Testing where return = Ret; (>>=) = Bind

data Testing a where
  Ret :: a -> Testing a
  Bind :: Testing a -> (a -> Testing b) -> Testing b
  T1 :: Test -> Testing ()

collect :: Testing () -> [Test]
collect m = loop m $ \_ -> [] where
  loop :: Testing a -> (a -> [Test]) -> [Test]
  loop m k = case m of
    Ret a -> k a
    Bind m f -> loop m $ \a -> loop (f a) k
    T1 x -> x : k ()
