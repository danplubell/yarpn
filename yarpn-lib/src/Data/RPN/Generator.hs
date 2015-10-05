module Data.RPN.Generator where

import Data.RPN.Internal.Types
import qualified Data.Set as DSE
import qualified Data.Sequence as DSEQ
import qualified Data.Foldable as DF

generate::Tree -> (DSEQ.Seq Code, DSE.Set Code) -> (DSEQ.Seq Code, DSE.Set Code)
generate t (cds,syms)=
  case t of
    AssignNode s l       ->
      let (cds' , syms') = generate l (cds,syms)
      in (cds' DSEQ.|> Movesym s, DSE.insert (Sym s) syms')
    SymbolNode s         ->
      (cds DSEQ.|> Pushsym s, DSE.insert (Sym s) syms)
    NumberNode n         ->
      (cds DSEQ.|> Push n , syms)
    SumNode PlusOp l r   ->
      let (cds', syms') = generate r (generate l (cds , syms))
      in (cds' DSEQ.|> Add 2, syms')
    SumNode MinusOp l r  ->
      let (cds', syms') = generate r (generate l (cds , syms))
      in (cds' DSEQ.|> Sub 2, syms')
    OperationNode MinOp l r ->
      let (cds', syms') = generate r (generate l (cds, syms))
      in (cds' DSEQ.|> Min 2, syms')
    OperationNode MaxOp l r ->
      let (cds', syms') = generate r (generate l (cds, syms))
      in (cds' DSEQ.|> Max 2, syms')
    UnaryNode PlusOp l       ->
      let (cds', syms') = generate l (cds,syms)
      in (cds' DSEQ.|> Pos 1, syms')
    UnaryNode MinusOp l ->
      let (cds', syms') = generate l (cds, syms)
      in (cds' DSEQ.|> Neg 1, syms') 
    ProdNode TimesOp l r      ->
      let (cds' , syms') = generate r (generate l (cds, syms))
      in (cds' DSEQ.|> Mul 2, syms')
    ProdNode DivOp l r ->
      let (cds',syms') = generate r (generate l (cds, syms))
      in (cds' DSEQ.|> Div 2, syms')
    ProdNode ModOp l r ->
      let (cds', syms') = generate r (generate l (cds, syms))
      in (cds' DSEQ.|> Mod 2, syms')
    ProdNode PowOp l r ->
      let (cds', syms') = generate r (generate l (cds, syms))
      in (cds' DSEQ.|> Pow 2, syms')
    _                  -> error "Error generating instructions"

emitInstructions::(DSEQ.Seq Code, DSE.Set Code) -> [String]
emitInstructions (cds,syms) = map show (DSE.toList syms) ++ map show (DF.toList cds)
