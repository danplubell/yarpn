module Data.RPN.Parser (parse,printTree) where

import           Data.RPN.Internal.Types

{-
Grammar
p0 ::= <p2> <p1>
p1 ::= '+' <p2> <p1>
   ::= '-' <p2> <p1>
   ::= e
p2 ::= <p4> <p3>
p3 ::= '*' <p4> <p3>
   ::= '/' <p4> <p3>
   ::= '%' <p4> <p3>
   ::= '^' <p4> <p3>
   ::= e
p4 ::= <p6> <p5>
p5 ::= 'min' <p6> <p5>
   ::= 'max' <p6> <p5>
   ::= e
p6 ::= '(' <p0> ')'
   ::= <symbol>
   ::= <number>
-}

p0::[Token]->(Tree,[Token])
p0 tokens =
  let (t , rest) = p2 tokens
      in p1 t rest

p1::Tree -> [Token] -> (Tree,[Token])
p1 t tokens =
  case lookAhead tokens of
    TokenAssign       -> case t of
                          SymbolNode s -> let (extree,rest) = p0 (accept tokens)
                                               in (AssignNode s extree,rest)
                          _            -> error $ "only variables can be assigned :  " ++ show t
    (TokenOp PlusOp)  -> let (r, rest) = p2 (accept tokens)
                         in p1 (SumNode PlusOp t r) rest
    (TokenOp MinusOp) -> let (r, rest) = p2 (accept tokens)
                          in p1 (SumNode MinusOp t r) rest
    _                 -> (t, tokens)

p2::[Token] -> (Tree,[Token])
p2 tokens = let (l , rest) = p4 tokens
                in p3 l rest

p3::Tree -> [Token] -> (Tree,[Token])
p3 t tokens = case lookAhead tokens of
  TokenOp TimesOp -> let (r , rest) = p4 (accept tokens)
                    in p3 (ProdNode TimesOp t r) rest
  TokenOp DivOp   -> let (r , rest) = p4 (accept tokens)
                    in p3 (ProdNode DivOp t r) rest
  TokenOp ModOp   -> let (r, rest) = p4 (accept tokens)
                    in p3 (ProdNode ModOp t r) rest
  TokenOp PowOp   -> let (r, rest) = p4 (accept tokens)
                    in p3 (ProdNode PowOp t r) rest
  _               -> (t, tokens)



p4::[Token] -> (Tree,[Token])
p4 tokens = let (l, rest) = p6 tokens
                in p5 l rest

p5::Tree -> [Token] -> (Tree,[Token])
p5 t tokens = case lookAhead tokens of
  TokenOper MinOp -> let (r, rest) = p6 (accept tokens)
                      in p5 (OperationNode MinOp t r) rest
  TokenOper MaxOp -> let (r, rest) = p6 (accept tokens)
                      in p5 (OperationNode MaxOp t r) rest
  _               -> (t, tokens)

p6::[Token] -> (Tree, [Token])
p6 tokens = case lookAhead tokens of
  TokenParen LeftParen            -> let (r, rest) = p0 (accept tokens)
                                    in
                                      if lookAhead rest /= TokenParen RightParen
                                        then error "Missing right paren"
                                        else (r, accept rest)
  TokenSymbol s                   -> (SymbolNode s, accept tokens)

  TokenNumber n                   -> (NumberNode n, accept tokens)

  (TokenOp op)
    | op `elem` [PlusOp, MinusOp] -> let (r, rest) = p6 (accept tokens)
                                    in (UnaryNode op r, rest)
  _                               -> error $  "Parse error on token: " ++ show tokens

parse::[Token] -> Tree
parse tokens = let (tree, rest) = p0 tokens
                   in
                      if null rest
                      then tree
                      else error $ "Leftover tokens: " ++ show tokens

lookAhead :: [Token] -> Token
lookAhead [] = TokEnd
lookAhead (c:_) = c

accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (_:ts) = ts


printTree::Tree -> [String]
printTree = showTree [] 0

showTree :: [String]->Int->Tree -> [String]
showTree strl level t =
         case t of
           SymbolNode s         -> showNode strl "SymbolNode" s
           NumberNode n'        -> showNode strl "NumberNode" (show n')
           SumNode op l r       -> traverseChildren op l r strl "SumNode"
           ProdNode op l r      -> traverseChildren op l r strl "ProdNode"
           OperationNode op l r -> traverseChildren op l r strl "OperationNode"
           UnaryNode op l       ->
                showNode (showTree strl nextPad l)   "UnaryNode" (show op)
           AssignNode op l     ->
                showNode (showTree strl nextPad l)   "AssignNode" (show op)
           where
              showNode::[String]-> String -> String -> [String]
              showNode strl' s' op' = if level  > 0
                              then (padding level ++ s' ++ "( " ++ op'  ++ ")"):strl'
                              else (s' ++ " " ++ op'):strl'
              nextPad::Int
              nextPad = level + 2

              traverseChildren::Show a => a -> Tree->Tree->[String]->String -> [String]
              traverseChildren op' l' r' strl' s'   = showNode (showTree (showTree strl' nextPad r') nextPad l') s' (show op')

              padding::Int -> String
              padding = flip replicate ' '

