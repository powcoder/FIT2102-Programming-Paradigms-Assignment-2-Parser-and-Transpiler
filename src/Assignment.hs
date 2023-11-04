https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
--This module contains the skeleton code for the assignment.
--
-- Please do not change the names of the parseExerciseX functions, as they
-- are used by the test suite.
--
-- You may, and are highly encouraged, to create your functions.
module Assignment where

import Instances (Parser (..))
import Parser (is, isNot, spaces, string, int, char, noneof, eof, stringTok, commaTok, charTok, tok, 
              sepBy, quoteString, op, space, digit, upper, lower)
import Control.Applicative (Alternative (many, some, (<|>)), Applicative (liftA2))
import Data.List (intercalate)

data ADT -- abstract syntax tree
  = AInt Int
  | ABool Bool
  | AList[ADT]
  | ANot String ADT -- unary operator !
  | ABinaryExpr ADT String ADT -- string operator and two ADT operands
  | ATernaryExpr ADT ADT ADT
  | AString String
  | AConst String ADT
  | ABlock [ADT] 
  | ACond String ADT String ADT
  | Empty
  deriving (Eq, Show)

isMultiline :: ADT -> [ADT] -> Bool
isMultiline input = longOutput <|> multipleChildren <|> childIsMultiline
  where 
    longOutput = length input > 42
    multipleChildren = length input > 1
    childIsMultiline = any isMultiline input

-- | Exercise A

parseExerciseA :: Parser ADT
parseExerciseA = adtValues <* eof

-- Exercise 1A --
-- parser for integers
adtInt :: Parser ADT
adtInt = AInt <$> tok int

adtBool :: Parser ADT
adtBool = ABool <$> ((stringTok "true" >> return True) <|> (stringTok "false" >> return False))

-- parser for lists of these data types (doesnt need to be same dt)
adtList :: Parser ADT
adtList = AList <$> (charTok '[' *> sepBy adtValues commaTok <* charTok ']')

-- Exercise 2A --
-- parser for unary operator !
adtNot :: Parser ADT
adtNot = ANot <$> (charTok '(' *> op "!") <*> (adtBool <* charTok ')')

-- parser for arithmetic operations: +, -, *, **, /
-- parser for comparison operators: ===, !==, >, <
adtBinaryExpr :: Parser ADT
adtBinaryExpr = do
  _ <- charTok '('
  opnd1 <- adtValues
  operator <- op "+" <|> op "-" <|> op "*" <|> op "**" <|> op "/" <|> 
              op "===" <|> op "!==" <|> op ">" <|> op "<" <|> op "&&" <|> op "||"
  opnd2 <- adtValues
  _ <- charTok ')'
  return $ ABinaryExpr opnd1 operator opnd2

-- Exercise 3A --
-- parser for ternary operations:= (<expression> ? <expression> : <expression>)
adtTernaryExpr :: Parser ADT
adtTernaryExpr = do
  _ <- charTok '('
  cond <- adtValues
  _ <- charTok '?'
  eIfTrue <- adtValues
  _ <- charTok ':'
  eIfFalse <- adtValues
  _ <- charTok ')'
  return $ ATernaryExpr cond eIfTrue eIfFalse

adtString :: Parser ADT
adtString = do
  _ <- charTok '\"'
  str <- quoteString
  _ <- charTok '\"'
  return $ AString str

-- ADT as a variable like jsonValues for JsonValues for list
adtValues :: Parser ADT
adtValues = adtTernaryExpr <|> adtBinaryExpr <|> adtInt <|> adtBool <|> adtList <|> adtNot <|> adtString

prettyPrintExerciseA :: ADT -> String
-- prettyPrintExerciseA (ATernaryExpr cond eIfTrue eIfFalse) = 
--   let sep = if isMultiline [cond, eIfTrue, eIfFalse] then "\n" else " " -- if multiline then 
--   in "(" ++ prettyPrintExerciseA cond ++ sep ++ "?" ++ sep 
--   ++ prettyPrintExerciseA eIfTrue ++ sep ++ ":" ++ sep
--   ++ prettyPrintExerciseA eIfFalse ++ ")"
prettyPrintExerciseA (ATernaryExpr cond eIfTrue eIfFalse) = 
  let sep = if isMultiline then "\n" else " " -- if multiline then 
  in "(" ++ prettyPrintExerciseA cond ++ sep ++ "?" ++ sep 
  ++ prettyPrintExerciseA eIfTrue ++ sep ++ ":" ++ sep
  ++ prettyPrintExerciseA eIfFalse ++ ")"
  where
    isMultiline = case cond of 
      ABinaryExpr {} -> True
      ATernaryExpr {} -> True
      _ -> False
prettyPrintExerciseA (ABinaryExpr left oper right) = prettyPrintExerciseA left ++ " " ++ oper ++ " " ++ prettyPrintExerciseA right
prettyPrintExerciseA (AInt i) = show i
prettyPrintExerciseA (ABool b) = if b then "true" else "false"
prettyPrintExerciseA (AList l) = "[" ++ intercalate "," (map prettyPrintExerciseA l)  ++ "]"
prettyPrintExerciseA (ANot oper opnd) = oper ++ prettyPrintExerciseA opnd
prettyPrintExerciseA (AString s) = show s

-- | Exercise B

parseExerciseB :: Parser ADT
parseExerciseB = adtValuesB <* eof

-- 1B: parser for a const declaration
adtConst :: Parser ADT
adtConst = do
  _ <- stringTok "const"
  _ <- space
  name <- many (upper <|> lower <|> digit <|> is '_')
  _ <- space
  _ <- charTok '='
  expr <- adtValues
  _ <- charTok ';'
  return $ AConst name expr 

-- 2B: parser for a code block
-- adtCodeBlock :: Parser ADT
-- adtCodeBlock = do
--   _ <- charTok '{'
--   _ <- spaces
--   line <- many adtValuesB
--   _ <- stringTok "}"
--   _ <- spaces
--   return $ ABlock line

-- 3B: parser for a conditional structure
-- adtConditional :: Parser ADT
-- adtConditional = do
--   _ <- stringTok "if"
--   _ <- space
--   _ <- charTok '('
--   cond <- adtBool
--   _ <- charTok ')'
--   _ <- space
--   _ <- adtCodeBlock
--   rest <- (do _ <- stringTok "else"
--               adtCodeBlock) 
--           <|> pure []
  
adtValuesB :: Parser ADT
adtValuesB = adtConst <|> adtValues

prettyPrintExerciseB :: ADT -> String
prettyPrintExerciseB (AConst name expr) = "const " ++ name ++ " = " ++ prettyPrintExerciseA expr ++ ";" 
--prettyPrintExerciseB a = prettyPrintExerciseA a -- for the trees implemented above

-- prettyPrintExerciseB (ABlock block) =
--   let sep = if isMultiline then "\n" else " "
--   in "{" ++ sep ++ prettyPrintExerciseB block ++ sep ++ "}"
--   where 
--     isMultiline = length (lines (show block)) > 10
--prettyPrintExerciseB (ACond cond block) = "if ( " ++ cond ++ " ) " ++ prettyPrintExerciseB block

-- prettyPrintExerciseA (ATernaryExpr cond eIfTrue eIfFalse) = 
--   let sep = if isMultiline then "\n" else " " -- if multiline then 
--   in "(" ++ prettyPrintExerciseA cond ++ sep ++ "?" ++ sep 
--   ++ prettyPrintExerciseA eIfTrue ++ sep ++ ":" ++ sep
--   ++ prettyPrintExerciseA eIfFalse ++ ")"
--   where
--     isMultiline = case cond of 
--       ABinaryExpr {} -> True
--       ATernaryExpr {} -> True
--       _ -> False

-- | Exercise C


-- This function should determine if the given code is a tail recursive function
isTailRecursive :: String -> Bool
isTailRecursive _ = False

parseExerciseC :: Parser ADT
parseExerciseC = pure Empty

-- 1C: parser for function calls
-- 2C: parser for function structures with return statement
-- 3C: check a function is tail recursive

prettyPrintExerciseC :: ADT -> String
prettyPrintExerciseC _ = "You must write a pretty printer!"