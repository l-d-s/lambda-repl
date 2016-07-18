module LambdaCalculus.Parser where

import LambdaCalculus.Terms

import Data.Attoparsec.Text
import Data.Char (ord)
import Control.Applicative ((<|>))


variableNumberParser = do
    x <- satisfy (inClass "a-z")
    i <- decimal <|> return 0
    return ((ord x - 97) + (i * 26))


variableParser = do
    i <- variableNumberParser
    return (Var i)


applicationParser = do
    char '('
    x <- expressionParser
    char ' '
    y <- expressionParser
    char ')'
    return (App x y)


-- lambda term
lambdaTermParser = do
    char '('
    i <- variableNumberParser
    char '.'
    y <- expressionParser
    char ')'
    return (Lam i y)


expressionParser =
    variableParser <|>
    applicationParser <|>
    lambdaTermParser


parseExpression = parseOnly (expressionParser <* endOfInput)
