module Data.RPN.Internal.Types (Token(..)
                               , Operator (..)
                               , Operation(..)
                               , Paren(..)
                               , Tree (..)
                               , Code (..)
                               , Symbol) where

data Operator = PlusOp | MinusOp | TimesOp | DivOp | ModOp | PowOp |NotOp deriving (Show,Eq)
data Operation = MaxOp | MinOp | NotOper deriving (Show,Eq)
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
     | TokenAssign
     | TokEnd
     deriving (Show, Eq)

data Tree =
       SymbolNode String
     | NumberNode Double
     | SumNode Operator Tree Tree
     | ProdNode Operator Tree Tree
     | OperationNode Operation Tree Tree
     | UnaryNode Operator Tree
     | AssignNode String Tree
  deriving (Show,Eq)

data Code =
    Sym String
  | Pushsym String
  | Push Double
  | Add Int
  | Sub Int
  | Mul Int
  | Div Int
  | Min Int
  | Max Int
  | Neg Int
  | Pos Int
  | Pow Int
  | Mod Int
  | Movesym String
  | Nop
  deriving (Show,Ord,Read,Eq)


{-
instance Show Code where
  show c = case c of
    Sym s     -> "sym " ++ s
    Pushsym s -> "pushsym " ++ s
    Movesym s -> "movesym " ++ s
    Push n    -> "push " ++ show n
    Add n     -> "add " ++ show n
    Sub n     -> "sub " ++ show n
    Mul n     -> "mul " ++ show n
    Div n     -> "div " ++ show n
    Min n     -> "min " ++ show n
    Max n     -> "max " ++ show n
    Neg n     -> "neg " ++ show n
    Pos n     -> "pos " ++ show n
    Pow n     -> "pow " ++ show n
    Mod n     -> "mod " ++ show n
    Nop       -> "nop "
-}
