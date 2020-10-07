module Utils where

import Data.Map
import Control.Monad.State

import AbsGrammar
import Definitions


mergeInts :: Value -> Value -> (Integer -> Integer -> Integer) -> Value
mergeInts (I x) (I y) f = (I (f x y))

mergeBools :: Value -> Value -> (Bool -> Bool -> Bool) -> Value
mergeBools (B x) (B y) f = (B (f x y))

checkBindingTypes :: Int -> Value -> Type Int -> StateMonad () 
checkBindingTypes p v t = do
  let errMsg = "Improper type passed to function"
  case v of 
    V -> error(errMsg ++ " at line " ++ show p)
    _ -> checkReturnTypes p v t errMsg

checkReturnTypes :: Int -> Value -> Type Int -> String -> StateMonad ()
checkReturnTypes p v t msg = do
  case (v, t) of
    (I _, Int _) -> return ()
    (B _, Bool _) -> return ()
    (S _, Str _) -> return ()
    (V, Void _) -> return ()
    _ -> error(msg ++ " at line " ++ show p) 

checkSameTypes :: Int -> Value -> Value -> StateMonad ()
checkSameTypes p v1 v2 = do
  case (v1, v2) of
    (I _, I _) -> return ()
    (B _, B _) -> return ()
    (S _, S _) -> return ()
    (_, _)     -> error("Same non-void types needed at line " ++ show p)

readMap :: Ord a => Int -> Map a b -> a -> b
readMap p m k =
  if member k m then m ! k
  else error("Undefined variable or function at line " ++ show p) 

deleteKey :: Integer -> StateMonad ()
deleteKey k = do
  (env, st) <- get
  let st' = Store (delete k (vst st)) (cst st)
  put (env, st')

updateArit :: Integer -> Cost -> Cost
updateArit v (Cost a b m) = Cost (a + v) b m

updateLogic :: Integer -> Cost -> Cost
updateLogic v (Cost a b m) = Cost a (b + v) m

updateMem :: Integer -> Cost -> Cost
updateMem v (Cost a b m) = Cost a b (m + v)

updateCostRec :: [Ident] -> (Cost -> Cost) -> StateMonad ()
updateCostRec [] _ = return ()
updateCostRec (h:t) f = do
  (env, st) <- get
  let c = (cst st) ! h
  updateCStore h $ f c

addArit :: StateMonad ()
addArit = do
  (env, st) <- get
  updateCostRec (cenv env) (updateArit 1)

addLogic :: StateMonad ()
addLogic = do
  (env, st) <- get
  updateCostRec (cenv env) (updateLogic 1)

addMem :: Integer -> StateMonad ()
addMem v = do
  (env, st) <- get
  updateCostRec (cenv env) (updateMem v)

updateVEnv :: Ident -> Loc -> StateMonad ()
updateVEnv id loc = do 
  (env, st) <- get  
  let venv' = insert id loc (venv env)
  put (Env venv' (penv env) (cenv env), st)

updatePEnv :: Ident -> Func -> StateMonad ()
updatePEnv id f = do 
  (env, st) <- get  
  let penv' = insert id f (penv env)
  put (Env (venv env) penv' (cenv env),  st)

updateCEnv :: Ident -> StateMonad ()
updateCEnv id = do
  (env, st) <- get 
  let cenv' = id:(cenv env)
  put (Env (venv env) (penv env) cenv', st)  

updateVStore :: Loc -> Value -> StateMonad ()
updateVStore loc val = do 
  (env, st) <- get  
  let vst' = insert loc val (vst st) 
  put (env, Store vst' (cst st))

updateCStore :: Ident -> Cost -> StateMonad ()
updateCStore id cost = do
  (env, st) <- get
  let cst' = insert id cost (cst st)
  put (env, Store (vst st) cst')

allocVal :: Ident -> Value -> Integer -> StateMonad ()
allocVal id val mem = do
  (env, st) <- get
  let l = newloc (vst st)
  updateVEnv id l
  updateVStore l val
  addMem mem

allocId :: Ident -> Loc -> StateMonad () 
allocId id l = updateVEnv id l

updateBreak :: StateMonad Bool
updateBreak = do
  (env, st) <- get
  let (I bv) = (vst st) ! breakValIdx
  if bv > 1 then do
    updateVStore breakValIdx $ I (bv-1)
    return True
  else if bv == 1 then do
    updateVStore breakValIdx $ I (bv-1)
    updateVStore breakPosIdx $ I 0
    return True
  else
    return False

updateContinue :: StateMonad () 
updateContinue = updateVStore contPosIdx (I 0)

getFirst :: Maybe (Int, Int) -> Int
getFirst Nothing = -1
getFirst (Just (x, _)) = x

updateVal :: Int -> Ident -> Value -> StateMonad ()
updateVal p id v = do
  (env, st) <- get
  let l = readMap p (venv env) id
  updateVStore l v

newloc :: (Map Loc b) -> Loc
newloc s = 
  if (Data.Map.null s) 
  then 0 
  else fst(findMax s) + 1