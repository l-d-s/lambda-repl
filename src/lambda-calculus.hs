module LambdaCalculus where

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
import Data.List (nub)

-- Data type for lambda terms.
-- Using `int` to index variables.
data LambdaTerm =
      Var Int
    | Lam Int LambdaTerm
    | App LambdaTerm LambdaTerm
    deriving (Eq)


-- Some examples --
ltId :: LambdaTerm
ltId = Lam 0 (Var 0)

ltA :: LambdaTerm
ltA = Var 0

ltRetB :: LambdaTerm
ltRetB = Lam 0 (Var 1)

ltFst :: LambdaTerm
ltFst = Lam 0 (Lam 1 (Var 0))

ltSnd :: LambdaTerm
ltSnd = Lam 0 (Lam 1 (Var 1))


-- Pretty printing. Could use a library for this. --

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

-- Computations --

{-
Good place to learn about different evaluation strategies I think.
-}

-- Alpha-conversion

freeVars :: LambdaTerm -> [ Int ]
freeVars lt =
    nub (freeVars' [] lt)
    where
        freeVars' boundVars x =
            case x of
                Var i ->
                    if i `elem` boundVars then [] else [i]

                Lam i y ->
                    freeVars' (i : boundVars) y

                App y z ->
                    freeVars' boundVars y ++ freeVars' boundVars z


isClosed :: LambdaTerm -> Bool
isClosed lt = null (freeVars lt)


-- Note: doesn't warn if j is free in lt
alphaRename' :: Int -> Int -> LambdaTerm -> LambdaTerm
alphaRename' i j lt =
    case lt of
        Var k
            | k == i ->
                Var j

            | otherwise ->
                Var k

        App x y ->
            App (alphaRename' i j x) (alphaRename' i j y)

        Lam k x
            | k == i ->
                Lam k x

            | otherwise ->
                Lam k (alphaRename' i j x)


alphaRename :: Int -> LambdaTerm -> LambdaTerm
alphaRename i lt =
    let j = maximum (freeVars lt) + 1
    in
        alphaRename' i j lt

-- Beta reduction --

isBetaRedex :: LambdaTerm -> Bool
isBetaRedex lt =
    case lt of
        App (Lam _ _) _ ->
            True

        _ ->
            False


hasBetaRedex :: LambdaTerm -> Bool
hasBetaRedex lt =
    case lt of
        Var _ ->
            False

        Lam _ a ->
            hasBetaRedex a

        App a b ->
            isBetaRedex (App a b) || hasBetaRedex a || hasBetaRedex b


betaReduce :: Int -> LambdaTerm -> LambdaTerm -> LambdaTerm
betaReduce i x y =
    case y of

        Var j ->
            if j == i then x else Var j

        App a b ->
            App (betaReduce i x a) (betaReduce i x b)

        Lam j a
            | j == i ->
                Lam j a

            | j `notElem` freeVars x ->
                Lam j (betaReduce i x a)

            -- capture-avoiding substitution
            | otherwise ->
                Lam j (betaReduce i x (alphaRename i a))

-- Evaluation --

-- Right innermost
applicativeReduce :: LambdaTerm -> LambdaTerm
applicativeReduce lt =
    case lt of
        Var i ->
            Var i

        Lam i a ->
            Lam i (applicativeReduce a)

        App (Lam i a) b
            | hasBetaRedex b ->
                App (Lam i a) (applicativeReduce b)

            | hasBetaRedex a ->
                App (Lam i (applicativeReduce a)) b

            | otherwise ->
                betaReduce i b a

        App a b
            | hasBetaRedex b ->
                App a (applicativeReduce b)

            | hasBetaRedex a ->
                App (applicativeReduce a) b

            | otherwise ->
                App a b


-- Left outermost
normalReduce :: LambdaTerm -> LambdaTerm
normalReduce lt =
    case lt of
        Var i ->
            Var i

        Lam i a ->
            Lam i (normalReduce a)

        App (Lam i a) b ->
            betaReduce i b a

        App a b
            | hasBetaRedex a ->
                App (normalReduce a) b

            | hasBetaRedex b ->
                App a (normalReduce b)

            | otherwise ->
                App a b

-- Test expressions

test0 :: LambdaTerm
test0 = Lam 0 (Var 0) -- (a.a)

test1 :: LambdaTerm
test1 = Lam 0 (App (Var 0) (Var 1)) -- (a.(a b))

test2 :: LambdaTerm
test2 = Lam 0 (App (Var 0) (Var 0)) -- (a.(a a))

-- Substitution

-- Eta-conversion
