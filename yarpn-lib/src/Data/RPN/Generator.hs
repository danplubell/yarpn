module Data.RPN.Generator (generate
                          , emitInstructions
                          , emitRawInstructions) where

import Data.RPN.Internal.Types
import qualified Data.Set as DSE
import qualified Data.Sequence as DSEQ
import qualified Data.Foldable as DF
import Control.Monad.State


type GenState = (DSEQ.Seq Code, DSE.Set Code)

type Generator a = State GenState a

-- | Generate a tuple of codified instructions and the set of symbols
generate::Tree -> (DSEQ.Seq Code, DSE.Set Code)
generate t = fst $ runState (generateCodes t) (DSEQ.empty,DSE.empty) 


generateCodes::Tree -> Generator (DSEQ.Seq Code, DSE.Set Code)
generateCodes t = do
   (cds, syms) <- get
   case t of
       AssignNode s l       -> do
         (cds', syms') <- generateCodes l
         updateState  (cds' DSEQ.|> Movesym s, DSE.insert (Sym s) syms')
       SymbolNode s         -> do
         updateState (cds DSEQ.|> Pushsym s, DSE.insert (Sym s) syms)
       NumberNode n         -> do
         updateState (cds DSEQ.|> Push n , syms)
       SumNode PlusOp l r   -> do
         _ <- generateCodes l
         (cds',syms') <- generateCodes r
         updateState (cds' DSEQ.|> Add 2, syms')
       SumNode MinusOp l r  -> do
         _ <- generateCodes l
         (cds', syms') <- generateCodes r
         updateState (cds' DSEQ.|> Sub 2, syms')
       OperationNode MinOp l r -> do
         _ <- generateCodes l
         (cds', syms') <- generateCodes r
         updateState (cds' DSEQ.|> Min 2, syms')
       OperationNode MaxOp l r -> do
         _ <- generateCodes l
         (cds', syms') <- generateCodes r
         updateState (cds' DSEQ.|> Max 2, syms')
       UnaryNode PlusOp l       -> do
         (cds', syms') <-  generateCodes l
         updateState (cds' DSEQ.|> Pos 1, syms')
       UnaryNode MinusOp l -> do
         (cds', syms') <-  generateCodes l
         updateState (cds' DSEQ.|> Neg 1, syms') 
       ProdNode TimesOp l r      -> do
         _ <- generateCodes l
         (cds' , syms') <- generateCodes  r
         updateState (cds' DSEQ.|> Mul 2, syms')
       ProdNode DivOp l r -> do
         _ <- generateCodes l
         (cds',syms') <-  generateCodes r
         updateState (cds' DSEQ.|> Div 2, syms')
       ProdNode ModOp l r -> do
         _ <- generateCodes l
         (cds',syms') <-  generateCodes r
         updateState (cds' DSEQ.|> Mod 2, syms')
       ProdNode PowOp l r -> do
         _ <- generateCodes l
         (cds', syms') <- generateCodes r
         updateState (cds' DSEQ.|> Pow 2, syms')
       _                  -> error "Error generating instructions"

   where updateState state' = do
           put state'
           return state'
{-        
generate'::(DSEQ.Seq Code, DSE.Set Code) -> Tree -> (DSEQ.Seq Code, DSE.Set Code)
generate' (cds,syms) t =
  case t of
    AssignNode s l       ->
      let (cds' , syms') = generate' (cds,syms) l
      in (cds' DSEQ.|> Movesym s, DSE.insert (Sym s) syms')
    SymbolNode s         ->
      (cds DSEQ.|> Pushsym s, DSE.insert (Sym s) syms)
    NumberNode n         ->
      (cds DSEQ.|> Push n , syms)
    SumNode PlusOp l r   ->
      let (cds', syms') = generate' (generate' (cds , syms) l) r
      in (cds' DSEQ.|> Add 2, syms')
    SumNode MinusOp l r  ->
      let (cds', syms') = generate' (generate' (cds , syms) l ) r
      in (cds' DSEQ.|> Sub 2, syms')
    OperationNode MinOp l r ->
      let (cds', syms') = generate' (generate' (cds, syms) l ) r
      in (cds' DSEQ.|> Min 2, syms')
    OperationNode MaxOp l r ->
      let (cds', syms') = generate' (generate' (cds, syms) l ) r
      in (cds' DSEQ.|> Max 2, syms')
    UnaryNode PlusOp l       ->
      let (cds', syms') = generate' (cds,syms) l
      in (cds' DSEQ.|> Pos 1, syms')
    UnaryNode MinusOp l ->
      let (cds', syms') = generate' (cds, syms) l
      in (cds' DSEQ.|> Neg 1, syms') 
    ProdNode TimesOp l r      ->
      let (cds' , syms') = generate' (generate' (cds, syms) l ) r
      in (cds' DSEQ.|> Mul 2, syms')
    ProdNode DivOp l r ->
      let (cds',syms') = generate' (generate' (cds, syms) l ) r
      in (cds' DSEQ.|> Div 2, syms')
    ProdNode ModOp l r ->
      let (cds', syms') = generate' (generate' (cds, syms) l ) r
      in (cds' DSEQ.|> Mod 2, syms')
    ProdNode PowOp l r ->
      let (cds', syms') = generate' (generate' (cds, syms) l ) r
      in (cds' DSEQ.|> Pow 2, syms')
    _                  -> error "Error generating instructions"
-}
emitInstructions::(DSEQ.Seq Code, DSE.Set Code) -> [String]
emitInstructions (cds,syms) = map show (DSE.toList syms) ++ map show (DF.toList cds)

emitRawInstructions::(DSEQ.Seq Code, DSE.Set Code) -> [Code]
emitRawInstructions (cds,syms) = DSE.toList syms ++ DF.toList cds
