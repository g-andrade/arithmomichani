-- MIT License
-- 
-- Copyright (c) 2020 Guilherme Andrade
-- 
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
-- 
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
-- 
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

module Lib
    ( run
    ) where

import qualified System.Environment   
import qualified Data.Char
import qualified Data.Foldable


data Expression = Number Integer |
                  Operation Operator [Expression]
                  deriving Show

data Operator = Sum | Subtraction | Multiplication | Division
                deriving Show

data ParserState = ReadOp |
                   ReadArgStart Operator [Expression] |
                   ReadArgDigits Operator [Expression] Integer |
                   ReturnExpression Expression
                   deriving Show

run :: IO ()
run = do
    args <- System.Environment.getArgs
    let expressionString = head args
    let initialParserState = ReadOp
    case parseExpression expressionString initialParserState of
        (_, (ReturnExpression expression)) -> do
            putStrLn ("Expression: " ++ (show expression))
            let result = evaluateExpression(expression)
            putStrLn ("Result: " ++ (show result))
        (_, finalParserState) ->
            error ("bad expression; state is " ++ (show finalParserState))

parseExpression :: String -> ParserState -> (String, ParserState)
parseExpression (' ':more) ReadOp = 
    parseExpression more ReadOp
parseExpression ('(':more) ReadOp = do
    let subState = ReadOp
    let (moreAfterSubExpression, subStateAfterSubExpression) = parseExpression more subState
    parseExpression moreAfterSubExpression subStateAfterSubExpression
parseExpression (char:more) ReadOp
        |  char == '+' || char == '-' 
        || char == '*' || char == '/' = do
    let operator = parseOperator char
    let newState = ReadArgStart operator []
    parseExpression more newState
parseExpression (' ':more) (ReadArgStart operator prevArgs) = 
    parseExpression more (ReadArgStart operator prevArgs)
parseExpression ('(':more) (ReadArgStart operator prevArgs) = do
    let subState = ReadOp
    let (moreAfterSubExpression, subStateAfterSubExpression) = parseExpression more subState
    case subStateAfterSubExpression of
        (ReturnExpression subExpression) -> do
            let newPrevArgs = subExpression : prevArgs
            let newState = (ReadArgStart operator newPrevArgs)
            parseExpression moreAfterSubExpression newState
        _ -> do
            error ("bad sub-expression; state is " ++ (show subStateAfterSubExpression))
parseExpression (')':more) (ReadArgStart operator prevArgs) = do
    let expressionArgs = reverse prevArgs
    let expression = Operation operator expressionArgs
    let newState = (ReturnExpression expression)
    (more, newState)
parseExpression "" (ReadArgStart operator prevArgs) = do
    let expressionArgs = reverse prevArgs
    let expression = Operation operator expressionArgs
    let newState = (ReturnExpression expression)
    ("", newState)     
parseExpression (char:more) (ReadArgStart operator prevArgs)
        | Data.Char.isDigit char = do
    let acc0 = toInteger (Data.Char.digitToInt char)
    let newState = ReadArgDigits operator prevArgs acc0
    parseExpression more newState
parseExpression (char:more) (ReadArgDigits operator prevArgs acc)
        | Data.Char.isDigit char = do
    let newAcc = (acc * 10) + toInteger (Data.Char.digitToInt char)
    let newState = ReadArgDigits operator prevArgs newAcc
    parseExpression more newState    
parseExpression (' ':more) (ReadArgDigits operator prevArgs acc) = do
    let newPrevArgs = Number acc : prevArgs
    let newState = ReadArgStart operator newPrevArgs
    parseExpression more newState
parseExpression (')':more) (ReadArgDigits operator prevArgs acc) = do
    let expressionArgs = reverse (Number acc : prevArgs)
    let expression = Operation operator expressionArgs
    let newState = (ReturnExpression expression)
    (more, newState)    
parseExpression "" (ReadArgDigits operator prevArgs acc) = do
    let expressionArgs = reverse (Number acc : prevArgs)
    let expression = Operation operator expressionArgs
    let newState = (ReturnExpression expression)
    ("", newState) 
parseExpression "" state =
    ("", state)
parseExpression _ state =
    error ("bad expression; state is " ++ (show state))

parseOperator :: Char -> Operator
parseOperator '+' = Sum
parseOperator '-' = Subtraction
parseOperator '*' = Multiplication
parseOperator '/' = Division                             
parseOperator _   = error "bad operator"

evaluateExpression :: Expression -> Double
evaluateExpression (Operation Sum args) | length args > 0 = do
    sum (map evaluateExpression args)
evaluateExpression (Operation Subtraction args) | length args > 1 = do
    let evaluatedArgs = map evaluateExpression args
    let minuend : subtrahends = evaluatedArgs
    foldl (-) minuend subtrahends
evaluateExpression (Operation Multiplication args) | length args > 1 = do
    product (map evaluateExpression args)
evaluateExpression (Operation Division args) | length args > 1 = do
    let evaluatedArgs = map evaluateExpression args
    let dividend : divisors = evaluatedArgs
    foldl (/) dividend divisors
evaluateExpression (Number integer) =
    fromIntegral integer
evaluateExpression (Operation op args) = do
    error ("Invalid number of argument(s) for " ++ (show op) 
        ++ " (" ++ (show (length args)) ++ ")")    
