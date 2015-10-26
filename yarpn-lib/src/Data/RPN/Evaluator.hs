module Data.RPN.Evaluator (evaluateCodes,loadCodes)  where
import           Control.Monad.State
import           Data.Fixed
import qualified Data.Map                as DM
import           Data.RPN.Internal.Types
import qualified Data.Sequence           as DS

type SymbolTab = DM.Map String (Maybe Double)

type Stack = DS.Seq Double

-- | type that will hold the symbol table and the stack
data EvalState = EvalState {symTab::SymbolTab, stack::Stack} deriving (Show, Eq,Ord)

-- |The evaluator as a Monad
type Evaluator a = State EvalState a

-- | Lookup a symbol from the symbol table
lookUpSym :: String -> Evaluator (Maybe Double)
lookUpSym str = do
  evalS <- get
  case DM.lookup str (symTab evalS) of
    Just v -> return v
    Nothing -> return Nothing

-- | Add a symbol to the symbol table
addSym :: String -> Maybe Double -> Evaluator ()
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

-- | Get the top valud off the stack
top::Evaluator Double
top = do
  evalS <- get
  let t = DS.index  (stack evalS) 0
  return t

-- | Evaluate a list of Codes representated by a string
evaluateCodes::[String] -> Double
evaluateCodes l = fst $ (evaluate .loadCodes) l

-- | Evaluate a list of codes and return the result and the current state
evaluate::[Code] -> (Double,EvalState)
evaluate l  = runState (evaluator l) (EvalState DM.empty DS.empty)

-- | Evaluator for a list of codes
evaluator::[Code]->Evaluator Double
evaluator l  = do
  mapM_ evaluateCode l
  top


-- | Evaluate a single code
evaluateCode :: Code -> Evaluator ()
evaluateCode c = case c of
  Sym s       -> do
    v <- lookUpSym s
    case v of
      Nothing -> addSym s Nothing
      Just _  -> return ()
  Pushsym s   -> do
    v <- lookUpSym s
    case v of
      Nothing -> error "Variable value is not defined"
      Just x  -> push x
  Push n      -> push n
  Add _       -> pop2apply (+)
  Sub _       -> pop2apply (-)
  Mul _       -> pop2apply (*)
  Div _       -> pop2apply (/)
  Min _       -> pop2apply min
  Max _       -> pop2apply max
  Neg _       -> popapplyUnary negate
  Pos _       -> popapplyUnary abs
  Pow _       -> pop2apply (**)
  Mod _       -> pop2apply mod'
  Movesym s   -> do
    v <- pop
    addSym s (Just v)
    push v
  _           -> error "An unknown instruction was encountered"

-- | Pop one element off stack and apply unary function
popapplyUnary:: (Double -> Double) -> Evaluator ()
popapplyUnary f = do
  v1 <- pop
  let v = f v1
  push v

-- | Pop 2 elements off stack and apply function
pop2apply:: (Double -> Double -> Double) -> Evaluator ()
pop2apply f = do
  v2 <- pop
  v1 <- pop
  let v = f v1 v2
  push v

-- | Load list of Codes from a list of string representations
loadCodes::[String]->[Code]
loadCodes = map readCode
  where readCode s = read s::Code
