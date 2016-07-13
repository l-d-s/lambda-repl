module Main where

import LambdaCalculus.Terms
import LambdaCalculus.ExampleTerms
import LambdaCalculus.Conversions


takeWhileDistinct :: Eq a => [a] -> [a]
takeWhileDistinct [] = []
takeWhileDistinct [x] = [x]
takeWhileDistinct [x, y] = if x == y then [x] else [x, y]
takeWhileDistinct (x:y:zs) =
    if x == y then [x] else x : takeWhileDistinct (y:zs)


printEvaluation :: (LambdaTerm -> LambdaTerm) -> LambdaTerm -> IO ()
printEvaluation evaluator lt =
    mapM_ print (takeWhileDistinct (iterate evaluator lt))


main :: IO ()
main = putStrLn "hello world"
