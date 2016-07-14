module LambdaCalculus.Conversions where

import LambdaCalculus.Terms

import Data.List (nub)

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

-- Eta-conversion

etaConvert :: LambdaTerm -> LambdaTerm
etaConvert lt =
    case lt of
        Lam i (App a (Var j)) ->
            if i == j && i `notElem` freeVars a
            then
                a
            else
                lt

        _ ->
            lt
