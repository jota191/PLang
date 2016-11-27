

-- | Memory for P
-- | Juan García Garland

module Memory where

-- | Variables in P are of the form Xk where k is an integer,
-- | The memory is a function from Int (the k idex) to Integer
-- | Since there is no way to declare variables, when an index is accesed for
-- | the first time the value is 0
-- | Performance is NOT important, this structure is O(n) in every operation


type Index = Int 
type Memory = [(Index,Integer)]



lookUp :: Index -> Memory -> Integer
lookUp i [] = 0
lookUp i ((i',v):ms) = if i==i'
                       then v
                       else lookUp i ms


update :: Index -> Integer -> Memory -> Memory
update i v []            = [(i,v)]
update i v (h@(i',_):ms) = if i == i'
                           then (i,v):ms
                           else h:update i v ms

