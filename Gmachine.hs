module Gmachine where

import Language
import Util

data Instruction
  = Pushglobal Name
  | Pushint Integer
  | Push Int
  | Mkap
  | Update Int
  | Pop Int
  | Unwind
  deriving Show

data Node
  = NNum Integer
  | NAp Addr Addr
  | NGlobal Int GmCode
  | NInd Addr
  deriving Show

type GmCode = [Instruction]
type GmStack = [Addr]
type GmHeap = Heap Node
type GmGlobals = [(Name, Addr)]
type GmStats = Int

type GmState = (GmCode, GmStack, GmHeap, GmGlobals, GmStats)
