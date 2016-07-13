module ExampleTerms where

import LambdaCalculus.Terms

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
