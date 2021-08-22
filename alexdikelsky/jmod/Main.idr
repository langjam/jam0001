module Main

import Control.App
import Control.App.FileIO
import System.Console.GetOpt
import System.File
import Eval
import Types
import Functions
import Parser
import System

ycomb : String
ycomb = "(λ (b) ((λ (f) (b (λ (x) ((f f) x)))) (λ (f) (b (λ (x) ((f f) x))))))"

test : String -> String
test s =
  case parseExpr s of
       Right (x, _) => case jValue x of
                            Left s => show s
                            Right s => "Execution failed with " ++ show s
       Left k => "Parse failed with " ++ show k

stringEither : Show a => Either a b -> Either String b
stringEither = bimap show id


main : IO ()
main = do
  putStr "Enter the filename: "
  fname <- getLine
  f <- readFile fname 
  out <- pure $ case stringEither f of
                     Left s => s
                     Right s => test s
  putStrLn out

  -- putStrLn $ show $ Array2 [[Natural 4], [Natural 5]]
  -- putStrLn $ show $ ConsList [Array2 [[Natural 4], [Natural 5]],  ConsList [Symbol "sfd"]]
  -- putStrLn $ show $ ConsList [Symbol "+"]
  -- putStrLn $ show $ jValue $ Symbol "sadf"
  -- putStrLn $ show $ jValue $ Quoted $ ConsList [Symbol "+", Array1 [Natural 4, Natural 4]]
  -- putStrLn $ show $ jValue $ ConsList [Symbol "+", Array1 [Natural 4]]
  -- putStrLn $ show $ jValue $ ConsList [Symbol "+", Array1 [Natural 4, Natural 9]]

  -- putStrLn $ show $ jValue $ 
  --      ConsList [Symbol "+", Array1 [Natural 4, Natural 9], 
  --                            Array1 [Natural 4, Natural 9]]
  -- putStrLn $ show $ jValue $ ConsList [Symbol "+", Array0 (Natural 4), Array0 (Natural 3),
  --                                                  Array0 (Natural 5)]

  -- putStrLn $ show $ jValue $ ConsList [Symbol "+", Array0 (Natural 1), Array1 [Natural 4, Natural 9]]

  -- putStrLn $ show $ jValue $ Array2 [[Natural 4, Natural 9], [Natural 5, Natural 6]]
  -- putStrLn $ show $ jValue $ ConsList [Symbol "+", Array0 (Natural 20), Array2 [[Natural 4, Natural 9], [Natural 5, Natural 6]]]

  -- putStrLn $ show $ jValue $ ConsList [Symbol "+", Array1 [Finite 1 3, Finite 2 3], Array1 [Finite 2 3, Finite 0 3]]

  -- putStrLn $ show $ jValue $ ConsList [Symbol "+", Array1 [Natural 4, Natural 2], Array0 (Natural 3)]

  -- putStrLn $ show $ jValue $ ConsList [Symbol "*", Array1 [Natural 4, Natural 2], Array0 (Natural 3)]

  -- putStrLn $ show $ jValue $ ConsList [Symbol "*", Array1 [Natural 4, Natural 2], Array0 (Natural 3)]

  -- putStrLn $ show $ jValue $ ConsList [Symbol "+", ConsList [Symbol "+", Array0 (Natural 1), Array1 [Natural 3, Natural 4]],
  --                                                  ConsList [Symbol "+", Array0 (Natural 5), Array1 [Natural 3, Natural 4]]]

  -- putStrLn $ show $ ConsList [Symbol "+", ConsList [Symbol "+", Array0 (Natural 1), Array1 [Natural 3, Natural 4]],
  --                                                  ConsList [Symbol "+", Array0 (Natural 5), Array1 [Natural 3, Natural 4]]]


  -- putStrLn $ show $ jValue $ ConsList [Symbol "i.", Array0 (Natural 4)]
  -- putStrLn $ show $ jValue $ ConsList [Symbol "i.", Array0 (Natural 0)]
  -- putStrLn $ show $ jValue $ ConsList [Symbol "i.", Array0 (Natural 1)]

  -- -- putStrLn $ show $ jValue $ ConsList [Symbol "+", ConsList [Symbol "+", Array0 (Natural 1), Array1 [Natural 3, Natural 4]]]
  -- -- putStrLn $ show $ ConsList [Symbol "+", ConsList [Symbol "+", Array0 (Natural 1), Array1 [Natural 3, Natural 4]]]

  -- 
  -- putStrLn $ show $ parseExpr "(asdf asdf)"
  -- putStrLn $ show $ parseExpr "[asdf asdf]"
  -- putStrLn $ show $ parseExpr "[1 3]"
  -- putStrLn $ show $ parseExpr "[[1 3] [4 5]]"
  -- putStrLn $ show $ parseExpr "[[1 3] [4 5] 4]"
  -- putStrLn $ show $ parseExpr "[1m3 4 m 4]"
  -- putStrLn $ show $ parseExpr "[1m3 4m4]"

  -- putStrLn $ test "(+ [1 3] [4 5])"
  -- putStrLn $ show $ parseExpr "(* 3 [4 5])"
  -- putStrLn $ test "(* 3 [4 5])"
  -- putStrLn $ test "(* 3 (i. 5))"
  -- -- putStrLn $ test "(* 1m10 (i. 5))"

  -- putStrLn $ test "(* 2m4 (i. 5))"
  -- putStrLn $ test "(+ 1m4 (* 2m4 (i. 5)))"

  -- putStrLn $ test "(+ 1 [0 3m4 6m2])"
  -- putStrLn $ test "(+ 1 [0 3m4 6m2])"
  -- putStrLn $ test "(i. 4)"
  -- putStrLn $ test "(+ 0m4 (i. 8))"
  -- putStrLn $ test "(+ 3 4)"
  -- putStrLn $ test "(/ + 0 [3 4 5 6])"
  -- putStrLn $ test "(/ * 1 [1 2 3 4])"
  -- putStrLn $ test "(/ * 1 [1 2 3 4])"
  -- putStrLn $ test "(/ * 1 [[1 2 3 4] [5 6 7 8]])"
  -- putStrLn $ test "(|: [[1 2 3 4] [5 6 7 8]])"
  -- putStrLn $ test "(/ * 1 (|: [[2 3 5] [7 11 13]]))"
  -- putStrLn $ test "(= 1 [[2 1 5] [7 11 13]])"
  -- putStrLn $ test "(= 1 [2 3 5])"
  -- putStrLn $ test "(* (+ 0m15 (i. 16)) (+ 0m15 (i. 16)))"
  -- putStrLn $ test "(+ 3 [0m5 1m5 2m5 3m5 4m5])"

  -- putStrLn $ test "(λ (x) (+ x 3))"
  -- putStrLn $ test "((λ (x) (+ x 3)) [4 5])"

  -- putStrLn $ test "((λ (x y) (+ x y)) 4 5)"
  -- putStrLn $ test "(λ (f g) (λ (x) (f (g x))))"
  -- putStrLn $ test "((λ (f g) (λ (x) (f (g x)))) (λ (x) (+ 1 x)) (λ (x) (* 2 x)))"
  -- putStrLn $ test "(((λ (f g) (λ (x) (f (g x)))) (λ (x) (/ + 0 x)) |:) [[3 4] [5 6]])"

  -- putStrLn $ test "(/ + 0 [1 2 3])"

  -- putStrLn $ test "(let ((a 3) (b 4)) (+ a b))"
  -- putStrLn $ test "((λ (a b) (+ a b)) 3 4)" 
  -- putStrLn $ test "(% 1m13 (+ 1 (i. 12)))"

  -- putStrLn $ test "(lift (=z (zip (+ 0m3 (i. 15)) (+ 0m5 (i. 15)))))"

  -- putStrLn $ test "(let ((fizz (lift (=z (zip (+ 0m3 (i. 15)) (+ 0m5 (i. 15))))))) (zip (* (car (cdr (|: fizz))) (i. 15)) (* (car (|: fizz)) (i. 15))))"

  -- putStrLn $ test $ "(let ((fizz (lift (=z (zip (+ 0m3 (i. 10)) (+ 0m5 (i. 10)) (+ 0m15 (i. 10)))))))" ++
  --                     "(+ (/ + 0 (* (car (cdr (|: fizz))) (i. 10))) (/ + 0 (* (car (|: fizz)) (i. 10)))))"

  -- putStrLn $ test $ "(let " ++ 
  -- "((fizz (|: (lift (=z (zip (+ 0m3 (i. 10)) (+ 0m5 (i. 10)) (+ 0m15 (i. 10))))))))" ++
  -- "(zip (* (car fizz) (i. 10)) (* (car (cdr fizz)) (i. 10)) (* (car (cdr (cdr fizz))) (i. 10))))"

  -- putStrLn $ test $ "((λ (n) (let " ++ 
  -- "((fizz (|: (=z (zip (+ 0m3 (i. n)) (+ 0m5 (i. n)) (+ 0m15 (i. n))))))) " ++ 
  -- "(zip (* (car (lift fizz)) (i. n)) (* (* (car (cdr (lift fizz))) (i. n)) (lift (not (car (cdr (cdr fizz))))))))) " ++
  -- "20)"

  -- putStrLn $ test $ "((λ (n) (/ + 0 (/ + 0 (let " ++ 
  -- "((fizz (|: (=z (zip (+ 0m3 (i. n)) (+ 0m5 (i. n)) (+ 0m15 (i. n))))))) " ++ 
  -- "(zip (* (car (lift fizz)) (i. n)) (* (* (car (cdr (lift fizz))) (i. n)) (lift (not (car (cdr (cdr fizz))))))))))) " ++
  -- "1000)"
  -- putStrLn $ test "(let ((fizz (lift (=z (zip (+ 0m3 (+ 1 (i. 15))) (+ 0m5 (+ 1 (i. 15)))))))) (+ (/ + 0 (* 5 (cdr (car (|: fizz))))) (/ + 0 (* 3 (car (|: fizz))))))"


  -- putStrLn $ test $ "((λ (n) (/ + 0 (/ + 0 " ++ 
  --  "(let " ++ 
  --    "((fizz (transpose (is-zero (zip (+ 0m3 (i. n)) (+ 0m5 (i. n)) (+ 0m15 (i. n))))))) " ++
  --   "(zip " ++
  --     "(* (car (lift fizz)) (i. n)) " ++
  --     "(* " ++
  --        "(* (car (cdr (lift fizz))) (i. n)) " ++
  --        "(lift (not (car (cdr (cdr fizz))))))))))) " ++
  --  "1000)"

  -- putStrLn $ test "(/ (λ (x y) (+ x y)) 0 [1 2 3])"
  -- putStrLn $ test "(/ (λ (x) (λ (y) (+ x y))) 0 [1 2 3])"

  -- putStrLn $ test ycomb
  -- putStrLn $ case parseExpr ycomb of
  --                 Left a => ?sdf
  --                 Right a => show a
