{-# LANGUAGE OverloadedStrings #-}

module Main where

import LambdaCalculus.Terms
import LambdaCalculus.Parser
import LambdaCalculus.ExampleTerms
import LambdaCalculus.Conversions

import Turtle -- (stdin, stdout, liftIO)
import Turtle.Options

import Data.Text as T


takeWhileDistinct :: Eq a => [ a ] -> [ a ]
takeWhileDistinct xs =
    case xs of
        [] ->
            []
        [x] ->
            [x]
        [x, y] ->
            if x == y then [x] else [x, y]
        (x:y:zs) ->
            if x == y then [x] else x : takeWhileDistinct (y:zs)


evaluationSequence ::
    (LambdaTerm -> LambdaTerm) -> LambdaTerm -> [ LambdaTerm ]
evaluationSequence evaluator lt =
    takeWhileDistinct (iterate evaluator lt)


showEval evaluator lt =
    Prelude.unlines (fmap show (evaluationSequence evaluator lt))


processInputText evaluator t =
    T.pack (either (\t -> "Error: " <> t)
                   (showEval evaluator)
                (parseExpression t))


argParser = switch "applicative" 'a' "Use applicative evaluation order."


main :: IO ()
main = do
    useApplicativeOrder <- options "Lambda calculus REPL" argParser

    let evaluator = if useApplicativeOrder
                        then
                            applicativeReduce
                        else
                            normalReduce

    stdout (fmap (processInputText evaluator) stdin)
