module Data.RPN.Parser (parse) where

import Data.RPN.Internal.Types

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
    (TokenOp Plus)  -> let (r, rest) = p2 (accept tokens)
                         in p1 (SumNode Plus t r) rest
    (TokenOp Minus) -> let (r, rest) = p2 (accept tokens)
                          in p1 (SumNode Minus t r) rest
    _               -> (t, tokens)

p2::[Token] -> (Tree,[Token])
p2 tokens = let (l , rest) = p4 tokens
                in p3 l rest

p3::Tree -> [Token] -> (Tree,[Token])
p3 t tokens = case lookAhead tokens of
  TokenOp Times -> let (r , rest) = p4 (accept tokens)
                    in p3 (ProdNode Times t r) rest
  TokenOp Div   -> let (r , rest) = p4 (accept tokens)
                    in p3 (ProdNode Div t r) rest
  TokenOp Mod   -> let (r, rest) = p4 (accept tokens)
                    in p3 (ProdNode Mod t r) rest
  TokenOp Pow   -> let (r, rest) = p4 (accept tokens)
                    in p3 (ProdNode Pow t r) rest
  _             -> (t, tokens)
                       
    

p4::[Token] -> (Tree,[Token])
p4 tokens = let (l, rest) = p6 tokens
                in p5 l rest

p5::Tree -> [Token] -> (Tree,[Token])
p5 t tokens = case lookAhead tokens of
  TokenOper Min -> let (r, rest) = p6 (accept tokens)
                      in p5 (OperationNode Min t r) rest
  TokenOper Max -> let (r, rest) = p6 (accept tokens)
                      in p5 (OperationNode Max t r) rest
  _             -> (t, tokens)

p6::[Token] -> (Tree, [Token])
p6 tokens = case lookAhead tokens of
  TokenParen LeftParen -> let (r, rest) = p0 (accept tokens)
                         in
                           if lookAhead rest /= TokenParen RightParen
                           then error "Missing right paren"
                           else (r, accept rest)
  TokenSymbol s        -> (SymbolNode s, accept tokens)
  TokenNumber n        -> (NumberNode n, accept tokens)
  (TokenOp op) | elem op [Plus, Minus] ->
                 let (r, rest) = p6 (accept tokens)
                 in (UnaryNode op r, rest)
  _                    -> error $  "Parse error on token: " ++ show tokens 
                           
parse::[Token] -> Tree
parse tokens = let (tree, rest) = p0 tokens
                   in
                      if null rest
                      then tree
                      else error $ "Leftover tokens: " ++ show tokens 

{-
p0:: [Token]-> (Tree,[Token])
p0 = undefined

p4::[Token]-> (Tree, [Token])
p4 tokens =
  let (p6Tree,rest) = p6 tokens
      in p5 ( p6Tree,rest

p5::[Token]-> (Tree,[Token])
p5 tokens =
  let (p6Tree, tokens') = p6 tokens
  in  
    case lookAhead tokens' of
      (TokenOper o) ->
       let (p5Tree, tokens'') = p5 (accept tokens')
       in (OperationNode o p5Tree p6Tree, tokens'')
      _            -> (p6Tree, tokens')

      
p6:: [Token]-> (Tree,[Token])
p6 tokens = 
  case lookAhead tokens of
     (TokenNumber n)        -> (NumberNode n, accept tokens)
     (TokenSymbol s)        -> (SymbolNode s, accept tokens)
     (TokenOper op) | elem op [Plus,Minus] ->
                      let (facTree, tokens') = p6 (accept tokens)
                      in (UnaryNode facTree, tokens')
     (TokenParen LeftParen) ->
       let (expTree,tokens') = p0 (accept tokens)
       in
         if lookAhead tokens' /= TokenParen RightParen
         then error "Missing right parenthesis"
         else (expTree, accept tokens')
     _                      -> error $ "Parse error on token: " ++ show tokens
-}
lookAhead :: [Token] -> Token
lookAhead [] = TokEnd
lookAhead (c:_) = c

accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (_:ts) = ts


