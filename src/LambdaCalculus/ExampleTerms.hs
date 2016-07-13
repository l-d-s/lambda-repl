module LambdaCalculus.ExampleTerms where

import LambdaCalculus.Terms

-- Some examples from
-- https://en.wikipedia.org/wiki/Lambda_calculus#Standard_terms

ltI :: LambdaTerm
ltI = Lam 0 (Var 0)

ltLittleOmega :: LambdaTerm
ltLittleOmega = Lam 0 (App (Var 0) (Var 0))

ltOmega :: LambdaTerm
ltOmega = App ltLittleOmega ltLittleOmega

ltVarA :: LambdaTerm
ltVarA = Var 0

ltVarB :: LambdaTerm
ltVarB = Var 1

ltTrue :: LambdaTerm
ltTrue = Lam 0 (Lam 1 (Var 0))

ltFalse :: LambdaTerm
ltFalse = Lam 0 (Lam 1 (Var 1))
