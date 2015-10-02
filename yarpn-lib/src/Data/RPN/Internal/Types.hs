module Data.RPN.Internal.Types (Token(..)
                               , Operator (..)
                               , Operation(..)
                               , Paren(..)
                               , Tree (..)
                               , showTree
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

padding::Int -> [Char]
padding = flip (replicate) ' '

showTree :: Int->Tree -> IO ()
showTree level t = do 
         case t of
           SymbolNode s         -> showNode "SymbolNode" s
           NumberNode n'        -> showNode "NumberNode" (show n')
           SumNode op l r       ->
             do showNode "SumNode" (show op)
                showTree (level + 1) l
                showTree (level + 1) r
           ProdNode op l r      ->
             do showNode "ProdNode" (show op)
                showTree (level + 1) l
                showTree (level + 1) r
           OperationNode op l r ->
             do showNode "OperationNode" (show op)
                showTree (level + 1) l
                showTree (level + 1) r
           UnaryNode op l       ->
             do showNode "UnaryNode" (show op)
                showTree (level + 1) l
           where
             showNode::String -> String -> IO()
             showNode s' op' = if level  > 0
                              then putStrLn $ padding level ++ "(" ++ s' ++ " " ++ op'  ++ ")" 
                              else putStrLn $ s' ++ " " ++ op'
