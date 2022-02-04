module Top (main) where

import Tests

main :: IO ()
main = do
  putStrLn "*explore-codegen*"
  Tests.run
