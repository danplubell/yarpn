module Data.RPN.Internal.Types (Token(..)
                               , Operator (..)
                               , Operation(..)
                               , Paren(..)
                               , Tree (..)
                               , Symbol) where

data Operator = Plus | Minus | Times | Div | Mod | Pow |NotOp deriving (Show,Eq)
data Operation = Max | Min | NotOper deriving (Show,Eq)
data Paren = LeftParen | RightParen | NotParen deriving (Show,Eq)

type Symbol = [Char]

data Token = 
       TokenParen Paren
     | TokenNumber Double 
     | TokenOp Operator
     | TokenOper Operation
     | TokenWhiteSpace
     | TokenSymbol Symbol
     | TokenInvalid [Char]       
     | TokEnd
     deriving (Show, Eq)

data Tree = 
       SymbolNode String
     | NumberNode Double
     | SumNode Operator Tree Tree
     | ProdNode Operator Tree Tree
     | OperationNode Operation Tree Tree
     | UnaryNode Operator Tree 
  deriving (Show,Eq)




