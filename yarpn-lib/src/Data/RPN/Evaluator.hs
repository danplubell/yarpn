module Data.RPN.Evaluator where
import Data.RPN.Internal.Types
import qualified Data.Map as DM
import qualified Data.Sequence as DS
import Control.Monad.State

type SymbolTab = DM.Map String Double

type Stack = DS.Seq Double

-- | type that will hold the symbol table and the stack
data EvalState = EvalState {symTab::SymbolTab, stack::Stack} deriving (Show, Eq,Ord)

-- |The evaluator as a Monad
type Evaluator a = State EvalState a  

-- | Lookup a symbol from the symbol table
lookUpSym :: String -> Evaluator Double
lookUpSym str = do
  evalS <- get
  case DM.lookup str (symTab evalS) of
    Just v -> return v
    Nothing -> error $ "Undefined variable"

-- | Add a symbol to the symbol table
addSym :: String -> Double -> Evaluator ()
addSym str val = do
  evalS <- get
  put $ EvalState (DM.insert str val (symTab evalS)) (stack evalS)
  return ()

-- | Push as value onto the stack
push :: Double -> Evaluator ()
push n = do
  evalS <- get
  put $ EvalState (symTab evalS) (n DS.<| stack evalS)
  return ()

-- | Pop a value off the stack
pop :: Evaluator Double
pop = do
  evalS <- get
  let x DS.:< xs = DS.viewl (stack evalS)
  put $ EvalState (symTab evalS) xs
  return x

evaluate :: Stack  -> SymbolTab -> Double -> Code -> Double
evaluate s st d c = case c of
  Sym s       -> undefined
  Pushsym s   -> undefined
  Push n      -> undefined
  Add n       -> undefined
  Sub n       -> undefined
  Mul n       -> undefined
  Div n       -> undefined
  Min n       -> undefined
  Max n       -> undefined
  Neg n       -> undefined
  Pos n       -> undefined
  Pow n       -> undefined
  Mod n       -> undefined
  Movesym s   -> undefined
  _           -> undefined
  
evaluator:: Stack  -> SymbolTab -> [Code] -> Double 
evaluator s st cl = foldl (evaluate s st)  0.0 cl 

     
