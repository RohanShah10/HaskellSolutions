module Alloc where

import Data.Maybe
import Data.List

import Types
import Examples


------------------------------------------------------
--
-- Part I
--
count :: Eq a => a -> [a] -> Int
count a lst = length (filter (==a) lst)

degrees :: Eq a => Graph a -> [(a, Int)]
degrees (items, edges) = map (\x -> (x,(count x k))) items
    where
        (i,j) = unzip edges
        k = i ++ j

neighbours :: Eq a => a -> Graph a -> [a]
neighbours item (items, edges)
    = i ++ j
      where 
          i = map snd (filter (\(a,b) -> ((a == item))) edges)
          j = map fst (filter (\(a,b) -> ((b == item))) edges)

removeNode :: Eq a => a -> Graph a -> Graph a
removeNode item (items, edges)
    = (i, j)
    where 
        i = filter (/=item) items
        j = filter (\(a,b) -> ((a/= item) && (b/=item))) edges

------------------------------------------------------
--
-- Part II
-- --
-- colourGraph :: (Ord a, Show a) => Int -> Graph a -> Colouring a
-- colourGraph maxColours graph@([], _) = []
-- colourGraph maxColours graph@(items, edges) = colour g' graph [1..maxColours] [] 
--   where 
--     toRemove = itemWithSmallestDegree graph
--     g' = removeNode (toRemove) graph

-- colour :: (Ord a, Show a) => Graph a -> Graph a -> [Int] -> Colouring a 
-- colour g'@(i', e') g@(i,e) maxColours cMap = undefined




-- itemWithSmallestDegree :: Eq a => Graph a -> a
-- itemWithSmallestDegree g@(i,e) = itemWithSmallestDegree' (degrees g)
--     where 
--         itemWithSmallestDegree' :: Eq a => [(a, Int)] -> a 
--         itemWithSmallestDegree' lst = fst(lst!!j)
--             where
--               j = fromJust (elemIndex (minimum (map snd lst)) (map snd lst))

-----------------------------------------------------
--
-- Part III
--
buildIdMap :: Colouring Id -> IdMap
buildIdMap [] = [("return", "return")]
buildIdMap ((s, i): xs)
  |i == 0 = [(s, s)] ++ buildIdMap xs
  |otherwise = [(s, aStr)] ++ buildIdMap xs
      where aStr = "R" ++ (show i)

buildArgAssignments :: [Id] -> IdMap -> [Statement]
buildArgAssignments [] _ = []
buildArgAssignments (i:is) imap  = [Assign d (Var c)] ++ buildArgAssignments is imap
    where 
        [(c,d)] = filter (\(a,b) -> a == i) (imap) 

--data Exp = Const Int | Var Id | Apply Op Exp Exp 
--         deriving (Eq, Show)

renameExp :: Exp -> IdMap -> Exp
-- Pre: A precondition is that every variable referenced in 
-- the expression is in the idMap. 
renameExp (Const a) imap = Const a

renameExp (Var id) imap = Var (head (snd (unzip j)))
  where j = filter (\(a,b) -> a == id) imap 

renameExp (Apply o e1 e2) imap = (Apply o (renameExp e1 imap) (renameExp e2 imap))

renameExp a imap = a

renameBlock :: Block -> IdMap -> Block
-- Pre: A precondition is that every variable referenced in 
-- the block is in the idMap. 
renameBlock block imap = map (\x -> renameStatement x imap) (block)
    where 
        renameStatement :: Statement -> IdMap -> Statement
        renameStatement (Assign id exp) imap = Assign (lookUp id imap) (renameExp exp imap)
        renameStatement (If e1 b1 b2) imap = If (renameExp e1 imap) (renameBlock b1 imap) (renameBlock b2 imap)
        renameStatement (While e1 b) imap = While (renameExp e1 imap) (renameBlock b imap)



renameFun :: Function -> IdMap -> Function
renameFun (f, as, b) idMap
  = (f, as, buildArgAssignments as idMap ++ renameBlock b idMap)

-----------------------------------------------------
--
-- Part IV
--
buildIG :: [[Id]] -> IG
buildIG 
  = undefined

-----------------------------------------------------
--
-- Part V
--
liveVars :: CFG -> [[Id]]
liveVars 
  = undefined

buildCFG :: Function -> CFG
buildCFG 
  = undefined
