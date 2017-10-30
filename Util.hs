module Util where

type Addr = Int
type Heap a = (Int, [Int], [(Int, a)])

hAlloc :: Heap a -> a -> (Heap a, Int)
hAlloc (size, (next : free), cts) a = ((size + 1, free, (next, a) : cts), next)
hAlloc _ _ = undefined

hLookup :: Heap a -> Int -> a
hLookup (_, _, cts) a =
  maybe (error ("Can't find node " ++ (show a) ++ " in heap")) id (lookup a cts)

hUpdate :: Heap a -> Int -> a -> Heap a
hUpdate (size, free, cts) n a = (size, free, (n, a) : remove n cts)

remove :: Int -> [(Int, a)] -> [(Int, a)]
remove n [] = error ("Attempt to update nonexistent address #" ++ show n)
remove n ((n', a) : cts)
  | n == n' = cts
  | otherwise = (n', a) : remove n cts
