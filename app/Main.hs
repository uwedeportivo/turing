module Main (main) where

import qualified Turing (createMachine)

main :: IO ()
main = do
  putStrLn $ "Hello, Haskell: Turing.createMachine=" ++ show (Turing.createMachine ['x', 'x', 'y', 'x'])

