module Main

import Eval
import Types

main : IO ()
main = do
  putStrLn $ show $ jValue $ Natural 3
  putStrLn $ show $ jValue $ Finite 3 4
  putStrLn $ show $ jValue $ Finite 3 4


