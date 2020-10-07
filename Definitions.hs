module Definitions where

import Control.Monad.State

import Data.Map
import Data.Int
import AbsGrammar

data Value = B Bool | I Integer | S String | V deriving (Eq, Ord)

instance Show Value where
  show (B v) = show v
  show (I v) = show v
  show (S v) = removeFirst . reverse . removeFirst . reverse $ v

data Cost = Cost {
  arit :: Integer,
  logic :: Integer,
  mem :: Integer
} deriving Show

type Loc = Integer
type Func = (TopDef Int, VEnv)

type VStore = Map Loc Value
type CStore = Map Ident Cost

type VEnv = Map Ident Loc
type PEnv = Map Ident Func 
type CEnv = [Ident]

data Env = Env {
  venv :: VEnv,
  penv :: PEnv,
  cenv :: CEnv
}

data Store = Store {
  vst :: VStore,
  cst :: CStore
}

breakValIdx = -1
breakPosIdx = -2
contPosIdx = -3
returnValIdx = -4

intSize = 4
boolSize = 1
stringSize = 1

type StateMonad = StateT (Env, Store) IO

emptyEnv :: Env
emptyEnv = Env {
  venv = fromList [],
  penv = fromList [],
  cenv = []
}

emptyStore :: Store
emptyStore = Store {
  vst = fromList [
    (breakValIdx, (I 0)),
    (breakPosIdx, (I 0)),
    (contPosIdx, (I 0))
  ],
  cst = fromList []
}

emptyCost :: Cost
emptyCost = Cost {
  arit = 0,
  logic = 0,
  mem = 0  
}

removeFirst :: String -> String
removeFirst [] = []
removeFirst (c:cs) = cs