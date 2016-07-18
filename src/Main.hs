module Main where

import LambdaCalculus.Terms
import LambdaCalculus.Parser
import LambdaCalculus.ExampleTerms
import LambdaCalculus.Conversions

import Turtle -- (stdin, stdout, liftIO)

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


showNormalEval :: LambdaTerm -> String
showNormalEval lt =
    Prelude.unlines (fmap show (evaluationSequence normalReduce lt))


processInputText :: Text -> Text
processInputText t =
    T.pack (either (\t -> "Error: " <> t) showNormalEval
                (parseExpression t))


main :: IO ()
main = stdout (fmap processInputText stdin)
