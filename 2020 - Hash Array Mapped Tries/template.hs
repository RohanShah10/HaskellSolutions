module Tries where

import Data.List hiding (insert)
import Data.Bits

import Types
import HashFunctions
import Examples

--------------------------------------------------------------------
-- Part I

-- Use this if you're counting the number of 1s in every
-- four-bit block...
bitTable :: [Int]
bitTable
  = [0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4]

countOnes :: Int -> Int
countOnes n = length (filter (== 1) (toBinary n))

toBinary :: Int -> [Int]
toBinary 0 = []
toBinary n =  (toBinary quotient) ++ [remainder]
    where
      (quotient, remainder) = quotRem n 2
  
toIntegerConverter :: [Int] -> Int
toIntegerConverter [] = 0
toIntegerConverter n = toInteger' (reverse n) 0
  where
    toInteger' :: [Int] -> Int -> Int
    toInteger' [] _ = 0
    toInteger' (x:xs) counter
      |x == 1 = 2^counter + (toInteger' xs (counter+1))
      |otherwise = toInteger' xs (counter+1)



countOnesFrom :: Int -> Int -> Int
countOnesFrom index n =  length (filter (==1) (drop ((length bin) - index) bin))
  where 
    bin = toBinary n

getIndex :: Int -> Int -> Int -> Int
getIndex num blockNum blockSize 
  =  toIntegerConverter correctBlocksRemoved
    where    
      binaryNum = toBinary num
      endBlocksRemoved = take (length (binaryNum) - (blockNum*blockSize)) binaryNum
      correctBlocksRemoved = drop (length (endBlocksRemoved) - blockSize) endBlocksRemoved

-- Pre: the index is less than the length of the list
replace :: Int -> [a] -> a -> [a]
replace index str replacer = (take index str) ++ [replacer] ++ drop (index + 1) str

-- Pre: the index is less than or equal to the length of the list
insertAt :: Int -> a -> [a] -> [a]
insertAt index adder str
 = (take (index) str) ++ [adder] ++ (drop (index) str)

--------------------------------------------------------------------
-- Part II

-- data Trie = Leaf [Int] | Node BitVector [SubNode]
--           deriving (Eq, Show)

--type BitVector = Int

-- data SubNode = Term Int | SubTrie Trie
--              deriving (Eq, Show)

sumTrie :: (Int -> Int) -> ([Int] -> Int) -> Trie -> Int
sumTrie f1 f2 (Leaf lst) = f2 lst
sumTrie f1 f2 (Node n []) = 0
sumTrie f1 f2 (Node n (s:ss)) = sum (map (\x -> sumTriesNodes f1 f2 x ) (s:ss))
  where
    sumTriesNodes :: (Int -> Int) -> ([Int] -> Int) -> SubNode -> Int
    sumTriesNodes f1 f2 (Term a) = f1 a
    sumTriesNodes f1 f2 (SubTrie t) = sumTrie f1 f2 t


--
-- If you get the sumTrie function above working you can uncomment
-- these three functions; they may be useful in testing.
--
trieSize :: Trie -> Int
trieSize t
  = sumTrie (const 1) length t

binCount :: Trie -> Int
binCount t
  = sumTrie (const 1) (const 1) t

meanBinSize :: Trie -> Double
meanBinSize t
  = fromIntegral (trieSize t) / fromIntegral (binCount t)


member :: Int -> Hash -> Trie -> Int -> Bool
member value hash trie blockSize = member' value binaryValue 0 hash trie blockSize 
  where 
    binaryValue = toBinary value

    member' :: Int -> [Int] -> Int -> Hash -> Trie -> Int -> Bool
    member' value binaryValue currentLevel hash trie blockSize 
      = undefined
        where
          blockBit = getIndex hash currentLevel blockSize
--------------------------------------------------------------------
-- Part III

insert :: HashFun -> Int -> Int -> Int -> Trie -> Trie
insert f maxDepth blockSize value (Leaf lst)
  |elem value lst = (Leaf lst)
  |otherwise = Leaf ((lst ++ [value]))

insert f maxDepth blockSize value t = insert' f maxDepth blockSize value 0 t
  where 
    insert' :: HashFun -> Int -> Int -> Int -> Int -> Trie -> Trie
    insert' f maxDepth blockSize value level (Leaf lst)
      |elem value lst = (Leaf lst)
      |otherwise = Leaf ((lst ++ [value]))
    
    insert' f maxDepth blockSize value level t@(Node a [s])
      |level == maxDepth - 1 = (Leaf [value])
      |a == 0 = insertAt level newNode t
        where
          newNode = (Node 1 [(Term v)])

buildTrie :: HashFun -> Int -> Int -> [Int] -> Trie
buildTrie
  = undefined
