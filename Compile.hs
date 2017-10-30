module Compile where

import Data.List

import Gmachine
import Language
import Util

compile :: Program -> GmState
compile program = (initialCode, [], heap, globals, 0)
  where (heap, globals) = buildInitialHeap program

buildInitialHeap :: Program -> (GmHeap, GmGlobals)
buildInitialHeap program = mapAccumR allocateSc (0, [1..30], []) compiled
  where compiled = map compileSc program

allocateSc :: GmHeap -> (Name, Int, GmCode) -> (GmHeap, (Name, Addr))
allocateSc heap (name, nargs, instns) = (heap', (name, addr))
  where (heap', addr) = hAlloc heap (NGlobal nargs instns)

compileSc :: (Name, [Name], Expr) -> (Name, Int, GmCode)
compileSc (name, env, body) = (name, length env, compileR body (zip env [0..]))

compileR :: Expr -> [(Name, Int)] -> GmCode
compileR e env = compileC e env ++ [Update d, Pop d, Unwind]
  where d = length env

compileC :: Expr -> [(Name, Int)] -> GmCode
compileC (EVar v) env
  | elem v (domain env) = [Push n]
  | otherwise  = [Pushglobal v]
  where
    n = maybe (error "Can't happen") id (lookup v env)
    domain list = [ key | (key, _) <- list]
compileC (ENum n) _ = [Pushint n]
compileC (EAp e1 e2) env
  = compileC e2 env ++ compileC e1 (argOffset 1 env) ++ [Mkap]

argOffset :: Int -> [(Name, Int)] -> [(Name, Int)]
argOffset n env = [(v, n + m) | (v, m) <- env]

initialCode :: GmCode
initialCode = [Pushglobal "main", Unwind]
