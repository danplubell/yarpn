module Data.RPN.Tokenizer (tokenize) where

import Data.RPN.Internal.Types
import Data.Char

-- | Take in a string of characters and tokenize them
   
-- | Convert a character to an abstract parenthese
paren :: Char -> Paren
paren c | c == '(' = LeftParen
        | c == ')' = RightParen
        | otherwise = NotParen

-- | Convert a character to an abstract operator      
operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Times
           | c == '/' = Div
           | c == '%' = Mod
           | c == '^' = Pow
           | otherwise = NotOp

-- | Convert a character to a token
tokenizeChar::Char -> Token 
tokenizeChar c 
  | elem c "()"    = TokenParen (paren c)
  | elem c "-+/*%" = TokenOp (operator c)
  | isSpace c      = TokenWhiteSpace
  | otherwise      = TokenInvalid $ "Cannot tokenize: " ++ [c]
                     
-- | Convert to an operation or identifier
identifier:: Char -> [Char] -> [Token]
identifier c cs = let (str,cs') = span isAlphaNum cs in
                        operationOrIdent (c:str) : tokenize cs'   
                  where operationOrIdent str' 
                          | str' == "min" = TokenOper Min
                          | str' == "max" = TokenOper Max
                          | otherwise    = TokenSymbol str'

-- | Convert to number representated as a double 
number :: Char -> [Char] -> [Token]                                                 
number c cs = 
   let (digs, cs') = span isDecimalOrDigit cs in
       cnvtToTokenNum (c:digs) : tokenize cs'
   where isDecimalOrDigit c' = isDigit c' || c' == '.'
         cnvtToTokenNum s = TokenNumber (read s::Double) 

-- | Convert a string to a list of tokens
tokenizer :: String -> [Token]
tokenizer [] = []
tokenizer (c:cs)
         | isDigit c = number c cs
         | isAlpha c = identifier c cs
         | otherwise = tokenizeChar c : tokenizer cs

-- | Convert a string to a filtered list of tokens
tokenize :: String -> [Token]
tokenize = filter (\t -> t /= TokenWhiteSpace) . tokenizer 
