module Interpreter where

import Data.Map
import Control.Monad.State

import AbsGrammar
import Definitions
import Semantic
import Utils

interpret :: Program (Maybe (Int, Int)) -> IO ()
interpret (Program _ decls funs) = do
  evalStateT (process (Prelude.map (fmap getFirst) decls) (Prelude.map (fmap getFirst) funs)) (emptyEnv, emptyStore)

defineFunctions :: [TopDef Int] -> StateMonad ()
defineFunctions [] = return ()
defineFunctions (h:t) = do
  semFunc h
  defineFunctions t

defineGlobalVars :: [Decl Int] -> StateMonad ()
defineGlobalVars [] = return ()
defineGlobalVars (h:t) = do 
  semDecl h
  defineGlobalVars t

process :: [Decl Int] -> [TopDef Int] -> StateMonad ()
process decls funs = do
  defineGlobalVars decls
  defineFunctions funs 
  semExpr (EApp 1 (Ident "main") [])
  (env, st) <- get
  if (Data.Map.null (cst st)) == False 
    then printCosts
    else return ()

printCost :: (Ident, Cost) -> StateMonad ()
printCost (ident, cost) = do
  lift $ putStrLn $ "Cost of " ++ show ident
  lift $ putStrLn $ "Arithmetic :\t" ++ show (arit cost) ++ " ops"
  lift $ putStrLn $ "Logical ops:\t" ++ show (logic cost) ++ " ops"
  lift $ putStrLn $ "Memory alloc:\t" ++ show (mem cost) ++ " B"

printCostRec :: [(Ident, Cost)] -> StateMonad ()
printCostRec [] = return ()
printCostRec (h:t) = do
  lift $ putStrLn ""
  printCost h
  printCostRec t

printCosts :: StateMonad ()
printCosts = do
  lift $ putStr "\n======================"
  (env, st) <- get
  printCostRec (assocs (cst st))

