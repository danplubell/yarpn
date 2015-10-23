module Data.RPN.Generator (generate
                          , emitInstructions
                          , emitRawInstructions) where

import           Control.Monad.State
import qualified Data.Foldable           as DF
import           Data.RPN.Internal.Types
import qualified Data.Sequence           as DSEQ
import qualified Data.Set                as DSE


type GenState = (DSEQ.Seq Code, DSE.Set Code)

type Generator a = State GenState a

-- | Generate a tuple of codified instructions and the set of symbols
generate::Tree -> (DSEQ.Seq Code, DSE.Set Code)
generate t = evalState (generateCodes t) (DSEQ.empty,DSE.empty)


generateCodes::Tree -> Generator (DSEQ.Seq Code, DSE.Set Code)
generateCodes t = do
   (cds, syms) <- get
   case t of
       AssignNode s l       -> do
         (cds', syms') <- generateCodes l
         updateState  (cds' DSEQ.|> Movesym s, DSE.insert (Sym s) syms')
       SymbolNode s         ->
         updateState (cds DSEQ.|> Pushsym s, DSE.insert (Sym s) syms)
       NumberNode n         ->
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
emitInstructions::(DSEQ.Seq Code, DSE.Set Code) -> [String]
emitInstructions (cds,syms) = map show (DSE.toList syms) ++ map show (DF.toList cds)

emitRawInstructions::(DSEQ.Seq Code, DSE.Set Code) -> [Code]
emitRawInstructions (cds,syms) = DSE.toList syms ++ DF.toList cds
