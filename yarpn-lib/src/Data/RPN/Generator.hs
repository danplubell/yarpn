module Data.RPN.Generator where

import Data.RPN.Internal.Types
import qualified Data.Set as DSE
import Data.Sequence


generate::Tree -> (Seq Code, DSE.Set Code) -> (Seq Code, DSE.Set Code)
generate t (cds,syms)=
  case t of
    SymbolNode s         ->
      (cds |> Pushsym s, DSE.insert (Sym s) syms)
    NumberNode n         ->
      (cds |> Push n , syms)
    SumNode PlusOp l r   ->
      let (cds', syms') = generate r (generate l (cds , syms))
      in (cds' |> Add 2, syms')
    SumNode MinusOp l r  ->
      let (cds', syms') = generate r (generate l (cds , syms))
      in (cds' |> Sub 2, syms')
    OperationNode MinOp l r ->
      let (cds', syms') = generate r (generate l (cds, syms))
      in (cds' |> Min 2, syms')
    OperationNode MaxOp l r ->
      let (cds', syms') = generate r (generate l (cds, syms))
      in (cds' |> Max 2, syms')
    UnaryNode PlusOp l       ->
      let (cds', syms') = generate l (cds,syms)
      in (cds' |> Pos 1, syms')
    UnaryNode MinusOp l ->
      let (cds', syms') = generate l (cds, syms)
      in (cds' |> Neg 1, syms') 
    ProdNode TimesOp l r      ->
      let (cds' , syms') = generate r (generate l (cds, syms))
      in (cds' |> Mul 2, syms')
    ProdNode DivOp l r ->
      let (cds',syms') = generate r (generate l (cds, syms))
      in (cds' |> Div 2, syms')
    ProdNode ModOp l r ->
      let (cds', syms') = generate r (generate l (cds, syms))
      in (cds' |> Mod 2, syms')
    ProdNode PowOp l r ->
      let (cds', syms') = generate r (generate l (cds, syms))
      in (cds' |> Pow 2, syms')
    _                  -> error "Error generating instructions"
