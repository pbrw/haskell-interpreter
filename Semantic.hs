module Semantic where

import Data.Map
import Control.Monad.State
import AbsGrammar
import Definitions
import Utils




-------------------- FUNCTIONS ---------------------

evalExprs :: [Expr Int] -> StateMonad [Value]
evalExprs [] = return []
evalExprs (h:t) = do
  v <- semExpr h
  res <- evalExprs t
  return $ v:res

bindValues :: Int -> [Value] -> [Arg Int] -> StateMonad VEnv
bindValues _ [] [] = return $ fromList []
bindValues p (vh:vt) (ah:at) = do
  let Arg _ typ id = ah
  checkBindingTypes p vh typ
  (env, st) <- get
  let 
    l = newloc (vst st)
    res = fromList [(id, l)]
  updateVStore l vh
  updateVEnv id l
  case vh of 
    (I _) -> addMem intSize
    (B _) -> addMem boolSize
    (S _) -> addMem stringSize
  recres <- bindValues p vt at
  return $ union res recres

bindReferences :: [Expr Int] -> [Arg Int] -> StateMonad VEnv
bindReferences [] [] = return $ fromList []
bindReferences (eh:et) (ah:at) = do
  recres <- bindReferences et at
  (env, st) <- get
  let (RefArg ap at aid) = ah
  case eh of
    EVar p id -> 
      let 
        loc = readMap p (venv env) id
        val = readMap p (vst st) loc
        res = fromList [(aid, loc)] in do
      checkBindingTypes p val at
      return $ union res recres
    _ -> error("Argument passed by reference must be a single variable at line " ++ show ap)

bindVars :: Int -> VEnv -> [Expr Int] -> [Arg Int] -> StateMonad ()
bindVars _ _ [] [] = return ()
bindVars p gvenv exps args = do
  checkNumberOfArguments p exps args
  (env, st) <- get
  let ((ve, va), (re, ra)) = splitBindings exps args
  vv <- evalExprs ve
  venv1 <- bindValues p vv va
  venv2 <- bindReferences re ra
  (_, st) <- get
  put ((Env (unions [venv1, venv2, gvenv]) (penv env) (cenv env)), st)

checkNumberOfArguments :: Int -> [a] -> [b] -> StateMonad () 
checkNumberOfArguments p s t = do
  if length s == length t
    then return ()
    else error("Wrong number of arguments passed to function at line " ++ show p)

splitBindings :: [Expr Int] -> [Arg Int] -> (([Expr Int], [Arg Int]), ([Expr Int], [Arg Int]))
splitBindings [] [] = (([], []), ([], []))
splitBindings (h:t) (hf:tf) =
  let ((ve, va), (re, ra)) = splitBindings t tf in
  case hf of
    Arg _ _ _ -> ((h:ve, hf:va), (re, ra))
    RefArg _ _ _ -> ((ve, va), (h:re, hf:ra))
  
checkBrkOrCont :: StateMonad ()
checkBrkOrCont = do
  (_, st) <- get
  let 
    (I brkPos) = (vst st) ! breakPosIdx
    (I contPos) = (vst st) ! contPosIdx in
    if brkPos > 0
      then error("Trying to break outside a loop at line " ++ show brkPos)
      else if contPos > 0
        then error("Continue outside a loop at line " ++ show contPos)
        else return ()

semExpr :: Expr Int -> StateMonad Value
semExpr (EApp p idd args) = do
  (env, st) <- get
  let ((FnDef _ t id fargs body), gvenv) = readMap p (penv env) idd
  bindVars p gvenv args fargs
  semBlock body
  checkBrkOrCont
  (_, st') <- get
  put (env, st')
  let res = case t of 
          Void _ -> V
          _ -> 
            if member returnValIdx (vst st') 
            then
              readMap p (vst st') returnValIdx
            else
              error("No return statement in non-void function at line " ++ show p)
  deleteKey returnValIdx
  checkReturnTypes p res t "Returned value of different type than expectd in function"
  return (res)



-------------------- EXPRESSIONS ---------------------

semExpr (EVar p id) = do
  (env, st) <- get
  return $ vst st ! readMap p (venv env) id

semExpr (ELitInt _ n) = do 
  return (I n)

semExpr (EAdd p e1 op e2) = do
  (v1, v2) <- getInts p e1 e2
  addArit
  return $ case op of
    Plus _ -> mergeInts v1 v2 (+)
    Minus _ -> mergeInts v1 v2 (-)

semExpr (EMul p e1 op e2) = do
  (v1, v2) <- getInts p e1 e2
  let (I y) = v2
  addArit
  return $ case op of
    Times _ -> mergeInts v1 (I y) (*)
    Div _ -> 
      if y == 0 then error("Division by 0 at line " ++ show p)
      else mergeInts v1 (I y) (div)
    Mod _ ->
      if y == 0 then error("Modulus by 0 at line " ++ show p)
      else mergeInts v1 (I y) (mod)

semExpr (Neg p e) = do 
  (v, _) <- getInts p e e
  let (I x) = v
  addArit
  return $ I (-x)

semExpr (ERel p e1 (EQU a) e2) = do
  (v1, v2) <- getSameTypes p e1 e2
  addLogic
  return $ B (v1 == v2)
 
semExpr (ERel p e1 (NE a) e2) = do
  (v1, v2) <- getSameTypes p e1 e2
  addLogic
  return $ B (v1 /= v2)

semExpr (ERel p e1 op e2) = do
  (v1, v2) <- getInts p e1 e2
  addLogic
  return $ case op of
    LTH _ -> B (v1 < v2)
    GTH _ -> B (v1 > v2)
    LE _ -> B (v1 <= v2)
    GE _ -> B (v1 >= v2)

-- -- Boolean expressions --
semExpr (ELitTrue _) = return $ B True

semExpr (ELitFalse _) = return $ B False

semExpr (EAnd p e1 e2) = do
  (b1, b2) <- getBools p e1 e2
  addLogic
  return $ mergeBools b1 b2 (&&)

semExpr (EOr p e1 e2) = do 
  (b1, b2) <- getBools p e1 e2
  addLogic
  return $ mergeBools b1 b2 (||)

semExpr (Not p e1) = do  
  (b1, _) <- getBools p e1 e1
  addLogic
  return $ mergeBools b1 b1 $ \x y -> not x

-- -- String expressions --
semExpr (EString p s) = return $ S s





-------------------- STATEMENTS ---------------------

semFunc :: TopDef Int -> StateMonad ()
semFunc (FnDef p t id args body) = do
  (env, _) <- get
  updatePEnv id $ (FnDef p t id args body, venv env)

escapeBlock :: Store -> Bool
escapeBlock st = 
  let
    (I br) = (vst st) ! breakValIdx
    (I cn) = (vst st) ! contPosIdx
  in (member returnValIdx (vst st)) || (br > 0) || (cn > 0) 
 
semBlock :: Block Int -> StateMonad ()
semBlock (Block p (h:t)) = do 
  semIns h
  (env, st) <- get
  if escapeBlock st then 
      return ()
  else semBlock (Block p t)
semBlock (Block _ _) = return ()

semIns :: Instr Int -> StateMonad ()
semIns (DInstr _ d) = do 
  semDecl d 

semIns (SInstr _ s) = do
  (env, _) <- get
  semStmt s
  (_, st) <- get
  put (env, st)

semItem :: Item Int -> Type Int -> StateMonad ()
semItem (NoInit p id) t = do
  case t of
    Int _ -> allocVal id (I 0) intSize
    Bool _ -> allocVal id (B False) boolSize
    Str _ -> allocVal id (S "") stringSize

semItem (Init p id e) t = do 
  semItem (NoInit p id) t
  semStmt (Ass p id e)

semDecl :: Decl Int -> StateMonad ()
semDecl (Var p typ []) = return ()
semDecl (Var p typ (h:t)) = do 
  semItem h typ
  semDecl (Var p typ t)
    
semDecl (CostBlk p id b) = do
  (env, st) <- get
  when (not $ elem id (cenv env)) $ updateCEnv id
  when (not $ member id (cst st)) $ updateCStore id emptyCost
  semBlock b

-- Statements 
semStmt :: Stmt Int -> StateMonad ()
semStmt (Ret p e) = do
  v <- semExpr e
  updateVStore returnValIdx v

semStmt (VRet p) = 
  updateVStore returnValIdx V


semStmt (Ass p id e) = do
  newVal <- semExpr e
  (env, st) <- get
  let 
    loc = readMap p (venv env) id
    curVal = readMap p (vst st) loc
  checkSameTypes p curVal newVal
  updateVStore loc newVal

semStmt (BStmt p b) = do
  (env, st) <- get
  semBlock b
  (_, st') <- get
  put (env, st')

semStmt (While p e s) = do
  cond <- getBool p e
  let B v = cond
  if v == True then do
    semStmt s
    escape <- updateBreak
    updateContinue
    if escape then 
      return ()
    else 
      semStmt (While p e s)
  else
      return ()

semStmt (CondElse p e s1 s2) = do 
  cond <- getBool p e 
  let B v = cond
  if v == True then 
    semStmt s1
  else
    semStmt s2

semStmt (Cond p e s1) = 
  semStmt $ CondElse p e s1 $ BStmt p $ Block p [] 

semStmt (SExp p e) = do
  _ <- semExpr e
  return ()

semStmt (Empty p) = return ()

semStmt (Break p) =
  semStmt (MultiBrk p (ELitInt p 1))

semStmt (MultiBrk p e) = do 
  v <- getInt p e
  updateVStore breakValIdx v
  updateVStore breakPosIdx (I (toInteger p))

semStmt (Continue p) = do
  updateVStore contPosIdx (I (toInteger p))

semStmt (Print p e) = do
  v <- semExpr e
  lift $ putStrLn $ show v





-------------------- TYPE CHECKING ---------------------

getBool :: Int -> Expr Int -> StateMonad Value
getBool p e = do
  v <- semExpr e
  case v of 
    B _ -> return v
    _ -> error("Invalid type (bool required) at line " ++ show p)

getBools :: Int -> Expr Int -> Expr Int -> StateMonad (Value, Value)
getBools p e1 e2 = do
  v1 <- getBool p e1
  v2 <- getBool p e2
  return (v1, v2)

getInt :: Int -> Expr Int -> StateMonad Value
getInt p e = do
  v <- semExpr e
  case v of 
    I _ -> return v
    _ -> error("Invalid type (int required) at line " ++ show p)

getInts :: Int -> Expr Int -> Expr Int -> StateMonad (Value, Value)
getInts p e1 e2 = do
  v1 <- getInt p e1
  v2 <- getInt p e2
  return (v1, v2)

getSameTypes :: Int -> Expr Int -> Expr Int -> StateMonad (Value, Value)
getSameTypes p e1 e2 = do
  v1 <- semExpr e1
  v2 <- semExpr e2
  checkSameTypes p v1 v2
  return (v1, v2)