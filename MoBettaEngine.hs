module MoBettaEngine where


import System.IO
import qualified Data.HashMap as HM -- easy lookup and update of variables
import Control.Monad.State
import Control.Applicative
import Data.Maybe (fromMaybe) -- using fromMaybe to simplify some code

import MoBettaAST

-- Env is a "dictionary" of (variable, int) pairs. We use this to model storing values in variables. The HM.Map type constructor supports lookup and update of variables.
type Env = HM.Map String Integer

-- We need an empty environment at the beginning of a computation when no variables have yet been used.
emptyEnv :: Env
emptyEnv = HM.fromList []

-- Declare a "computation" to combine IO and ability to deal with an environment.
-- StateT combines a state type (in this case Env) and an existing monad (IO) to produce a monad in which a computation is essentially an IO computation that can access and modify the state.

type Computation t = StateT Env IO t



type Action = Computation ()
type IntCalc = Computation Integer
type BoolCalc = Computation Bool


statementAction :: Statement -> Action
statementAction (Print e) = printAction (intCalc e) -- display result of calculating e
statementAction (Msg s) = msgAction s -- display string s
statementAction (Read v) =  readAction v -- read in a value for v
statementAction (If b s1 s2) =
  ifAction (boolCalc b) (statementAction s1) (statementAction s2)
                        -- Calculate b, then decide which computation to do
statementAction (While b s) = whileAction (boolCalc b) (statementAction s)
                        -- compute "while b s"
statementAction (Assign v e) = assignAction v (intCalc e)
                        -- compute e, and assign to v
statementAction (Block ls) = blockAction $ map statementAction ls
                        -- compute a sequence of individual computations
                        -- by translating each into a computation

makeProgram ls = blockAction $ map statementAction ls

{---------------------------------------------------------------------------
Some helpers to manipulate the state and to access IO.
----------------------------------------------------------------------------}

-- This turns an IO  into a Computation
-- We need this to perform IO within the Computation monad.
-- In a Computation block, write "doIO print" instead of "print", etc.
doIO :: IO a -> Computation a
doIO = lift


updateEnv :: String -> Integer -> Computation ()
updateEnv name val = modify $ HM.insert name val


retrieveEnv :: String -> Computation Integer
retrieveEnv name = do
  val <- gets $ HM.lookup name
  return $ fromMaybe (varNotFound name) val
  where
    varNotFound name = error $ "Identifier \"" ++ name ++ "\" not defined."


readAction :: String -> Action
readAction v = do
  x <- doIO getInt
  updateEnv v x
  where
    getInt = do
      inp <- getLine
      return $ read inp

-- Display a string
msgAction :: String -> Action
msgAction s = doIO $ putStr s

-- Display result of computing an integer
printAction :: IntCalc -> Action
printAction intCalc = do
  n <- intCalc -- execute given integer calculation to obtain an Integer
  doIO $ putStr $ show n -- display it

-- Compute an integer, then store it
assignAction :: String -> IntCalc -> Action
assignAction v intCalc = do
  n <- intCalc -- calculate the right-hand side of an assignment
  updateEnv v n -- store the result

-- Compute a boolean, use it to decide which computation to do.
ifAction :: BoolCalc -> Action -> Action -> Action
ifAction boolCalc action1 action2 = do
  cond <- boolCalc -- calculate the test condition
  if cond then     -- decide what to do
    action1
  else
    action2

whileAction :: BoolCalc -> Action -> Action
whileAction boolCalc action = do
  cond <- boolCalc -- calculate the test
  when cond loop   -- do the body of the loop
  where
    loop = do
      action
      whileAction boolCalc action

-- Do a list of actions sequentially.
-- NB. There is a combinator for this, but I've written it out for clarity.
blockAction :: [Action] -> Action
blockAction [] = return ()
blockAction (a:ls) = do
  a
  blockAction ls


aBinOps =
  [ (Add, (+))
  , (Sub, (-))
  , (Mul, (*))
  , (Div, div)
  , (Mod, mod)]

aUnOps =  [(Neg, negate)]

bBinOps =
  [ (And, (&&))
  , (Or, (||))]

bUnOps =  [(Not, not)]

relnOps =
  [ (Greater, (>))
  , (GreaterEqual, (>=))
  , (Less, (<))
  , (LessEqual, (<=))
  , (Equal, (==))
  , (NEqual, (/=))]


opLookup :: Eq const => const -> [(const, sem)] -> sem
opLookup op opTable =
  fromMaybe (error "Undefined operator. Should never happen.")
            (lookup op opTable)

-- This defines the translation of a BExpr into a computation of Booleans
boolCalc :: BExpr -> BoolCalc
boolCalc (BoolConst b) = return b
boolCalc (Reln cOp expr1 expr2)
  = liftA2 (opLookup cOp relnOps) (intCalc expr1) (intCalc expr2)
boolCalc (BBin op expr1 expr2)
  = liftA2 (opLookup op bBinOps) (boolCalc expr1) (boolCalc expr2)
boolCalc (BUn op expr)
  = fmap (opLookup op bUnOps) (boolCalc expr)

intCalc :: AExpr -> IntCalc
intCalc (Var v) = retrieveEnv v
intCalc (IntConst val) = return val
intCalc (ABin op expr1 expr2)
  = liftA2 (opLookup op aBinOps) (intCalc expr1) (intCalc expr2)
intCalc (AUn op expr)
  = fmap (opLookup op aUnOps) (intCalc expr)
