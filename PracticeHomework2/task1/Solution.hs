module Solution where

import Data.Maybe
import Data.Char
import Data.List.Split

data Expr a
  = Atom a
  | Add (Expr a) (Expr a)
  | Subtr (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)
  | Div (Expr a) (Expr a)
  deriving (Show, Eq)

eval :: (Num a, Integral a) => Expr a -> Maybe a
eval (Atom number) = Just number
eval (Add a b) = Just (fromMaybe 0 (eval a) + fromMaybe 0 (eval b))
eval (Subtr a b) = Just (fromMaybe 0 (eval a) - fromMaybe 0 (eval b))
eval (Mul a b) = Just (fromMaybe 0 (eval a) * fromMaybe 0 (eval b))
eval (Div a b)
  | b /= (Atom 0) = Just (fromMaybe 0 (eval a) `div` fromMaybe 0 (eval b))
  | otherwise = Nothing

evaluateExpr :: (Num a, Integral a) => Char -> a -> a -> Expr a
evaluateExpr operator first second
  | operator == '+' = (Add (Atom first) (Atom second))
  | operator == '-' = (Subtr (Atom first) (Atom second))
  | operator == '*' = (Mul (Atom first) (Atom second))
  | operator == '/' = (Div (Atom first) (Atom second))
  | otherwise = Atom first

isOperator :: Char -> Bool
isOperator char = char `elem` "+-/*"

getWholeNumber :: [Char] -> Int -> Int
getWholeNumber [] currNumber = currNumber
getWholeNumber (h:t) currNumber =
  if isDigit h then getWholeNumber t (10 * currNumber + (digitToInt h))
  else currNumber

getNumberLength :: [Char] -> Int
getNumberLength [] = 0
getNumberLength (h:t) = if isDigit h then 1 + getNumberLength t else 0

evaluateStringHelper :: (Num a, Integral a) => String -> [a] -> [Char] -> [Expr a] -> Maybe a
evaluateStringHelper str numbers ops results
  | str == "" && (length numbers == 1) = Just (head numbers)
  | str == "" && (length results /= 1)= Nothing
  | str == "" = (eval (last results))
  | head str == ')' =
    let operator = last ops
        firstOperand = last (init numbers)
        secondOperand = last numbers
        result = (evaluateExpr operator firstOperand secondOperand)
        evaluatedResult = eval(result)
        newNumbers = (init (init numbers)) ++ [fromMaybe 0 evaluatedResult]
        newOps = init ops
        newResults = results ++ [result]
    in
      if evaluatedResult /= Nothing
      then evaluateStringHelper (tail str) newNumbers newOps newResults
      else Nothing
  | isDigit (head str) =
    let currNumber = getWholeNumber str 0
        numberLength = getNumberLength str
    in evaluateStringHelper (drop (numberLength-1) (tail str)) (numbers ++ [fromIntegral(currNumber)]) ops results
  | isOperator (head str) = evaluateStringHelper (tail str) numbers (ops ++ [(head str)]) results
  | otherwise = evaluateStringHelper (tail str) numbers ops results

bracketsEven :: String -> Integer
bracketsEven [] = 0
bracketsEven str
  | head str == '(' = (bracketsEven (tail str)) + 1
  | head str == ')' = (bracketsEven (tail str)) - 1
  | str == [] = 0
  | otherwise = (bracketsEven (tail str))

evaluateString :: String -> Maybe Integer
evaluateString str =
  if (bracketsEven str /= 0) then Nothing
  else evaluateStringHelper str [] [] []
