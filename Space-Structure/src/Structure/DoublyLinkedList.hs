module DoublyLinkedList where

data List = Empty | Node { getPoint :: (Int,Int) , left :: List , right :: List } deriving Show

genCycle :: [(Int,Int)] -> List
genCycle []     = Empty
genCycle (p:ps) = n0
    where
        n0 = Node p solidLastOne nextNode
        (solidLastOne, nextNode) = betweenLast ps n0 n0

        betweenLast :: [(Int,Int)] -> List -> List -> (List,List)
        betweenLast []     lastNode headNode = (lastNode, headNode)
        betweenLast [p]    lastNode headNode = let n2 = Node p lastNode headNode in (n2, n2)
        betweenLast (p:ps) lastNode headNode = (theLast, n1)
            where
                n1 = Node p lastNode nextNode
                (theLast, nextNode) = betweenLast ps n1 headNode

