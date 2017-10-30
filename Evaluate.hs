module Evaluate where

import Gmachine
import Language
import Util

eval :: GmState -> GmState
eval state@([], _, _, _, _) = state
eval state = eval $ doAdmin (step state)

doAdmin :: GmState -> GmState
doAdmin (c, st, h, g, s) = (c, st, h, g, s + 1)

step :: GmState -> GmState
step (i : is, st, h, g, s) = dispatch i (is, st, h, g, s)
step state = state

dispatch :: Instruction -> GmState -> GmState
dispatch (Pushglobal f) = pushglobal f
dispatch (Pushint n) = pushint n
dispatch (Push n) = push n
dispatch Mkap = mkap
dispatch (Update n) = update n
dispatch (Pop n) = pop n
dispatch Unwind = unwind

pushglobal :: Name -> GmState -> GmState
pushglobal f (code, stack, heap, globals, stats)
  = (code, addr : stack, heap, globals, stats)
  where addr = maybe (error ("Undeclared global " ++ f)) id (lookup f globals)

pushint :: Integer -> GmState -> GmState
pushint n (code, stack, heap, globals, stats)
  = (code, addr : stack, heap', globals, stats)
  where (heap', addr) = hAlloc heap (NNum n)

push :: Int -> GmState -> GmState
push n (code, stack, heap, globals, stats)
  = (code, addr : stack, heap, globals, stats)
  where addr = getArg $ (hLookup heap) (stack !! (n + 1))

        getArg (NAp _ addr2) = addr2
        getArg node = error ("Cannot getArg of " ++ show node)

mkap :: GmState -> GmState
mkap (code, addr1 : addr2 : stack, heap, globals, stats)
  = (code, addr : stack, heap', globals, stats)
  where (heap', addr) = hAlloc heap (NAp addr1 addr2)
mkap state = state

update :: Int -> GmState -> GmState
update n (code, addr : stack, heap, globals, stats)
  = (code, stack, heap', globals, stats)
  where heap' = hUpdate heap (stack !! n) (NInd addr)
update _ state = state

pop :: Int -> GmState -> GmState
pop n (code, stack, heap, globals, stats)
  = (code, drop n stack, heap, globals, stats)

unwind :: GmState -> GmState
unwind state@(code, addr : stack, heap, globals, stats) =
  newState (hLookup heap addr)
  where
    newState (NNum _) = state
    newState (NAp addr1 _)
      = (Unwind : code, addr1 : addr : stack, heap, globals, stats)
    newState (NGlobal n c)
      | length stack < n = error "Unwinding with too few arguments"
      | otherwise = (c, addr : stack, heap, globals, stats)
    newState (NInd addr')
      = (Unwind : code, addr' : stack, heap, globals, stats)
unwind state = state
