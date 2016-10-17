-- This uses the PriorityQueue module to heap sort a list
module HeapSort where

import qualified PriorityQueue as Q

sort :: Ord a => [a] -> [a]
sort lst = helper (Q.fromList lst) []
    where
        helper :: Ord a => Q.Pqueue a -> [a] -> [a]
        helper hp lst =
            case Q.popMin hp of
                Nothing -> reverse lst
                Just (minElem, newHp) -> helper newHp (minElem : lst)
