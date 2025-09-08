module Evaluator
  ( evalProg,
    runEval,
  )
where

import AST
import Data.List (lookup, deleteBy)
import Data.Bits ((.&.), (.|.), xor)
import Control.Monad (ap,liftM)

data Val
  = ValInt Int
  | ValArr [Int]
  deriving (Eq,Show)


type Store =  [(String,Val)]
type FTable = [(String,Exp)]
type Error = String


newtype EvalM a = EvalM (FTable -> Store -> Either Error (Store, a))

instance Functor EvalM where
  fmap = liftM

instance Applicative EvalM where
  pure x = EvalM $ \_ftable store -> Right (store, x)
  (<*>) = ap

instance Monad EvalM where
  EvalM x >>= f = EvalM $ \ftable store ->
    case x ftable store of
      Left err -> Left err
      Right (store',x') ->
        let EvalM y = f x'
         in y ftable store'


getStore :: EvalM Store
getStore = EvalM $ \_ store -> Right (store,store)

putStore :: Store -> EvalM ()
putStore store = EvalM $ \_ftab _store -> Right (store,())

getFtab :: EvalM FTable
getFtab = EvalM $ \ftable store -> Right (store,ftable)

failure :: String -> EvalM a
failure s = EvalM $ \_ftab _store -> Left s


-- | Looks up a valid address in the store, given an key.
storeLookup :: String -> EvalM Val
storeLookup s = do
  store <- getStore
  case lookup s store of
    Just x -> pure x
    Nothing -> pure $ ValInt 0


-- | Update a store value. Duplicates are replaced.
put :: String -> Val -> EvalM ()
put s val = do
  store <- getStore
  putStore $ (s,val) 
    : (deleteBy (\(k1,_) (k2,_) -> k1 == k2) (s,val) store)


-- | Looks up a store value and updates it using a predefined function.
-- In other words, combines lookup and put into a single function.
updateByKey :: String -> (Val -> Val) -> EvalM ()
updateByKey s f = do
  val <- storeLookup s
  put s $ f val


-- | Array inplace update
replace :: (Int, a) -> [a] -> [a]
replace _ [] = []
replace (i, e) arr =
  take i arr ++ [e] ++ drop (i+1) arr


-- | Applies some update to an address, given a value as input.
--applyToAddr :: (Int -> Int -> Int) -> Int -> Exp -> EvalM ()
--applyToAddr f v e@(Var vname) = do
--  addrVal <- lookupAddr e
--  store <- getS
--  case addrVal of
--    ValInt x -> 
--      putS $ 
--        (vname,ValInt $ f x v) 
--        : (deleteBy (\(k1,_) (k2,_) -> k1 == k2) (vname,ValInt v) store)
--    _ -> failure "123"
--applyToAddr f val (ArrIdx vname e) = do
--  v <- evalArith e
--  store <- getS
--  case lookup vname store of
--    Just (ValArr arr) | v < (length arr) && v > 0 -> do
--      putS $ 
--        (vname,ValArr $ replace (v,f (arr!!v) val) arr) 
--        : (deleteBy (\(k1,_) (k2,_) -> k1 == k2) (vname,ValInt val) store)
--    Just (ValArr arr) -> 
--      failure $ "Out-of-bounds exception; "
--        ++ vname ++ " has length " ++ show (length arr) ++
--        " but indexing was " ++ show v
--    _ -> failure $ vname ++ " is not an array!"
--applyToAddr _ _ _ = failure "Programmer, an address has to be a variable or array index..."


runEval :: EvalM a -> a
runEval (EvalM m) = 
  case m [] [] of
    Right (_,x) -> x
    Left err -> error err


evalBinOp :: (a -> a -> b) -> (c -> EvalM a) -> c -> c -> EvalM b
evalBinOp op f e1 e2 = do
  x <- f e1
  y <- f e2
  pure $ op x y


evalArith :: Arith -> EvalM Int
evalArith (Const x) = pure x
evalArith (Var vname) = do 
  val <- storeLookup vname
  case val of
    ValInt x -> pure x
    _ -> failure $ "Variable " ++ vname ++ " is initialized and is not an int"
evalArith (ArrIdx vname e) = do
  e' <- evalArith e
  val <- storeLookup vname
  case val of
    ValArr arr ->
      if e' < (length arr) && e' > 0
        then pure $ arr!!e'
        else failure $ "Out-of-bounds; length of " 
          ++ vname ++ " is " ++ length arr ++ " while indexation was "
          ++ show e'
    _ -> failure $ "Variable " ++ show val ++ " is not an array"
evalArith (Add e1 e2) = do evalBinOp (+) evalArith e1 e2
evalArith (Sub e1 e2) = do evalBinOp (-) evalArith e1 e2
evalArith (Tim e1 e2) = do evalBinOp (*) evalArith e1 e2
evalArith (Div e1 e2) = do evalBinOp div evalArith e1 e2
evalArith (Mod e1 e2) = do evalBinOp mod evalArith e1 e2
evalArith (Xor e1 e2) = do evalBinOp xor evalArith e1 e2
evalArith (BAnd e1 e2) = do evalBinOp (.&.) evalArith e1 e2
evalArith (BOr e1 e2) = do evalBinOp (.|.) evalArith e1 e2
evalArith unmatched =
  failure $ "Found expression "
    ++ show unmatched ++
    " in place of an arithmetic expression"


evalAssert :: Assert -> EvalM Bool
evalAssert (Eq e1 e2) = do evalBinOp (==) evalArith e1 e2
evalAssert (Geq e1 e2) = do evalBinOp (>=) evalArith e1 e2
evalAssert (Leq e1 e2) = do evalBinOp (<=) evalArith e1 e2
evalAssert (Gt e1 e2) = do evalBinOp (>) evalArith e1 e2
evalAssert (Lt e1 e2) = do evalBinOp (<) evalArith e1 e2
evalAssert (And e1 e2) = do evalBinOp (&&) evalAssert e1 e2
evalAssert (Or e1 e2) = do evalBinOp (||) evalAssert e1 e2
evalAssert unmatched =
  failure $ "Found expression "
    ++ show unmatched ++
    " in place of an assertive expression"




evalStmt :: Stmt -> EvalM ()
evalStmt (AddAss vname e) = do
  e' <- evalArith e
  updateByKey vname (\x -> x + e')
evalStmt (SubAss vname e) = do
  e' <- evalArith e
  updateByKey vname (\x -> x + e')
evalStmt (XorAss vname e) = do
  e' <- evalArith e
  updateByKey vname (\x -> x + e')
evalStmt (TimAss vname e) = do
  e' <- evalArith e
  updateByKey vname (\x -> x + e')
evalStmt (DivAss vname e) = do
  e' <- evalArith e
  updateByKey vname (\x -> x + e')
evalStmt (If a1 e1 e2 a2) = do
  a1' <- evalAssert a1
  if a1'
    then do evalStmt e1
    else do evalStmt e2
  a2' <- evalAssert a2
  if a1' == a2'
    then pure ()
    else failure $ 
      "Exit assertion " ++ show a1 ++ 
      " did not match initial condition " ++ show a2
evalStmt (From a1 e1 e2 a2) = do
  a1' <- evalAssert a1
  if a1'
    then do evalStmt e1
    else do evalStmt e2
  a2' <- evalAssert a2
  case (a1',a2') of
    (a,b) | a == b -> pure ()
    _ -> failure $ "Exit assertion " ++ show a1 ++ " did not match initial condition " ++ show a2
evalStmt Skip = pure ()
evalStmt (Seq e1 e2) = do
  evalStmt e1
  evalStmt e2
evalStmt (Swap v1 v2) = do undefined
--  val1 <- lookupAddr v1
--  val2 <- lookupAddr v2
--  applyToAddr (\x _ -> x) val1 $ Var v1
--  applyToAddr (\x _ -> x) val2 $ Var v2
evalStmt unmatched = 
  failure $ "Found expression "
    ++ show unmatched ++
    " in place of a statement"


evalProg :: Exp -> EvalM ()
evalProg e = evalStmt e
