module LambdaCalculus.Terms where

{-
Sources of inspiration:

-  [John's Lambda Calculus and Combinatory Logic Playground]
   (http://tromp.github.io/cl/cl.html). Would be fun to implement some
   of the pretty printing etc. from here.

-  Tromp, J. (2014). Binary lambda calculus and combinatory logic.
   Randomness and Complexity, from Leibniz to Chaitin 237–260.

-  Clean syntax is from [Alberto Ruiz]
   (http://dis.um.es/~alberto/pages/lambdac.html).

-  Idea: `diagrams` backend.
-}

import Data.Char (chr)

-- Data type for lambda terms.
-- Using `int` to index variables.
data LambdaTerm =
      Var Int
    | Lam Int LambdaTerm
    | App LambdaTerm LambdaTerm
    deriving (Eq)


showVarIndex :: Int -> String
showVarIndex i =
    let
        letterNo     = i `mod` 26
        charNo       = letterNo + 97
        timesThrough = i `div` 26
    in
        case timesThrough of
            0 ->
                [chr charNo]

            _ ->
                -- slightly ugly but GHCi suggests not [x] ++ xs
                chr charNo : show timesThrough


showClean :: LambdaTerm -> String
showClean l =
    case l of
        Var x ->
            showVarIndex x -- fix to allow for a_1 etc.

        Lam x y ->
            "(" ++ show (Var x) ++ "." ++ show y ++ ")"

        App x y ->
            "(" ++ show x ++ " " ++ show y ++ ")"


showTrad :: LambdaTerm -> String
showTrad l =
    case l of
        Var x ->
            showVarIndex x -- fix to allow for a_1 etc.

        Lam x y ->
            "(\\" ++ show (Var x) ++ "." ++ show y ++ ")"

        App x y ->
            "(" ++ show x ++ " " ++ show y ++ ")"


instance Show LambdaTerm where
    show = showClean
