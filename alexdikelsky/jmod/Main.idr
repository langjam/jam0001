module Main

import Eval
import Types
import Functions

main : IO ()
main = do
  putStrLn $ show $ jValue $ Natural 3
  putStrLn $ show $ jValue $ Finite 3 4
  putStrLn $ show $ jValue $ Finite 3 4
  putStrLn $ show $ jValue $ Quoted $ Array [Natural 3, Natural 4]
  putStrLn $ show $ jValue $ Quoted $ Array [Symbol "+", Natural 3, Natural 4]
  putStrLn $ show $ jValue $ Array [Symbol "+", Natural 3, Natural 4]
  putStrLn $ show $ jValue $ Array [Symbol "+", Natural 3, Finite 2 4]


