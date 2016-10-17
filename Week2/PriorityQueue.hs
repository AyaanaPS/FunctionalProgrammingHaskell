-- This impelements the PriorityQueue module by defining all of the functions
-- specified (and a few extra for help)
module PriorityQueue(Pqueue, 
                     empty, 
                     isEmpty, 
                     insert, 
                     findMin,
                     deleteMin,
                     popMin,
                     fromList,
                     isValid)
where

    -- Datatype for priority queues, parameterized around an element type a.
    -- The element type should be an instance of the Ord type class.
    data Pqueue a = Leaf | Node a Int (Pqueue a) (Pqueue a)

    -- An empty priority queue storing values of type a.
    empty :: Pqueue a
    empty = Leaf

    -- Return True if the queue is empty.
    isEmpty :: Pqueue a -> Bool
    isEmpty Leaf = True
    isEmpty _ = False

    -- This returns the rank of the inputted heap. If the input is Leaf, the
    -- rank is 0.
    rank :: Pqueue a -> Int
    rank Leaf = 0
    rank (Node _ rank _ _) = rank

    -- This merges two inputted heaps.
    merge :: Ord a => Pqueue a -> Pqueue a -> Pqueue a
    merge Leaf a = a
    merge a Leaf = a
    merge hp1@(Node elem1 rank1 left1 right1) hp2@(Node elem2 rank2 left2 right2) =
        if elem1 < elem2 then newHeap elem1 left1 (merge right1 hp2)
        else newHeap elem2 left2 (merge right2 hp1)

        where
            newHeap :: Ord a => a -> Pqueue a -> Pqueue a -> Pqueue a
            newHeap element hp1 hp2 = 
                if (rank hp1) > (rank hp2) then (Node element ((rank hp2) + 1) hp1 hp2)
                else (Node element ((rank hp1) + 1) hp2 hp1)

    -- Insert an item into a priority queue.            
    insert :: Ord a => a -> Pqueue a -> Pqueue a
    insert element hp = merge hp (Node element 1 Leaf Leaf)

    -- Find the minimum-valued element in a priority queue if possible.
    findMin :: Ord a => Pqueue a -> Maybe a
    findMin Leaf = Nothing
    findMin (Node element _ _ _) = Just element

    -- Delete the minimum element from a priority queue if possible.
    deleteMin :: Ord a => Pqueue a -> Maybe (Pqueue a)
    deleteMin Leaf = Nothing
    deleteMin (Node _ _ left right) = Just (merge left right)

    -- Remove the minimum element if possible and return it, 
    -- along with the rest of the priority queue.
    popMin :: Ord a => Pqueue a -> Maybe (a, Pqueue a)
    popMin Leaf = Nothing
    popMin (Node element _ left right) = Just (element, merge left right)

    -- Convert an unordered list into a priority queue.
    fromList :: Ord a => [a] -> Pqueue a
    fromList [] = Leaf
    fromList (hd:tl) = insert hd (fromList tl)

    --Validate the internal structure of the priority queue.
    isValid :: Ord a => Pqueue a -> Bool
    isValid Leaf = True
    isValid (Node element rankNum left right) = 
        (rankNum >= 0) && ((rank left) >= (rank right)) && 
        (rankNum == ((rank right) + 1)) && 

        (case (findMin left) of 
            Nothing -> True 
            (Just a) -> element <= a) 
        &&
        (case (findMin right) of 
            Nothing -> True
            (Just a) -> element <= a) 

        &&
        (isValid left) && 
        (isValid right)
