
-- | Memory for P
-- | Juan García Garland

module Memory (Index,
               Memory,
               lookUp,
               update,
               emptyMemory,
               singletonMemory) where

-- | Variables in P are of the form Xk where k is an integer,
-- | The memory is a function from Int (the k idex) to Integer
-- | Since there is no way to declare variables, when an index is accesed for
-- | the first time the value is 0
-- | Performance is NOT important, this structure is O(n) in every operation

type Index = Int 
newtype Memory = M { runM :: [(Index,Integer)]}
               deriving Show

lookUp :: Index -> Memory -> Integer
lookUp i (M []) = 0
lookUp i (M ((i',v):ms)) = if i==i'
                           then v
                           else lookUp i (M ms)

update :: Index -> Integer -> Memory -> Memory
update i v (M [])            = M [(i,v)]
update i v (M (h@(i',_):ms)) = M (if i == i'
                                  then (i, v) : ms
                                  else h : runM (update i v (M ms)))

{- TOD0: test the stricter:    if i == i'
                               then M $ (i,v):ms
                               else M (h: ((\(M s) -> s) (update i v (M ms))))-}
{- TODO:
reimplement update, so indexes are ordered (or maybe use a dictionary) -}

emptyMemory :: Memory
emptyMemory = M []


-- these function/s is/are not abstract
singletonMemory :: Index -> Integer -> Memory
singletonMemory i v = update i v emptyMemory


-- tests
memtest = M [(0,1),(1,3),(2,5),(3,5),(4,5)]

