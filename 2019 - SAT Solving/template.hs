module SOL where

import Data.List
import Data.Maybe

import Types
import TestData

printF :: Formula -> IO()
printF
  = putStrLn . showF
  where
    showF (Var v)
      = v
    showF (Not f)
      = '!' : showF f
    showF (And f f')
      = "(" ++ showF f ++ " & " ++ showF f' ++ ")"
    showF (Or f f')
      = "(" ++ showF f ++ " | " ++ showF f' ++ ")"

--------------------------------------------------------------------------
-- Part I

-- 1 mark
lookUp :: Eq a => a -> [(a, b)] -> b
-- Pre: The item being looked up has a unique binding in the list
lookUp a b = fromJust (lookup a b)

-- 3 marks
vars :: Formula -> [Id]
vars a = sort (nub (vars' a))
  where
    vars' :: Formula -> [Id]
    vars' (Var id) = [id]
    vars' (Not formula) = vars formula
    vars' (And formula1 formula2) = vars formula1 ++ vars formula2 
    vars' (Or formula1 formula2) = vars formula1 ++ vars formula2

-- 1 mark
idMap :: Formula -> IdMap
idMap formula = zip (vars formula)  [1..]

--------------------------------------------------------------------------
-- Part II

-- An encoding of the Or distribution rules.
-- Both arguments are assumed to be in CNF, so the
-- arguments of all And nodes will also be in CNF.
distribute :: CNF -> CNF -> CNF
distribute a (And b c)
  = And (distribute a b) (distribute a c)
distribute (And a b) c
  = And (distribute a c) (distribute b c)
distribute a b
  = Or a b

-- 4 marks
toNNF :: Formula -> NNF
toNNF (Not (Or f1 f2)) = And (toNNF (Not f1)) (toNNF (Not f2))
toNNF (Not (And f1 f2)) = Or (toNNF (Not f1)) (toNNF (Not f2))
toNNF (Not (Not f)) = toNNF f
toNNF (Not f) = (Not f)
toNNF (And f1 f2) = And (toNNF f1) (toNNF f2)
toNNF (Or f1 f2) = Or (toNNF f1) (toNNF f2)
toNNF (formula) = formula

-- 3 marks
toCNF :: Formula -> CNF
toCNF formula = toCNF' (toNNF formula)
    where 
        toCNF' :: Formula -> CNF
        toCNF' (Or form1 (And form2 form3)) = distribute form1 (And form2 form3)
        toCNF' (Or (And form1 form2) (form3)) = distribute (And form1 form2) form3
        toCNF' f = f
          
-- 4 marks
flatten :: CNF -> CNFRep
flatten formula = flatten' formula (idMap formula)
    where
        flatten' :: CNF -> IdMap -> CNFRep
        flatten' (And f1 f2) map = [[lookUp f1 map], [lookUp f2 map]]
        flatten' (Or f1 f2) map = [[lookUp f1 map, lookUp f2 map]]
--------------------------------------------------------------------------
-- Part III

-- 5 marks
propUnits :: CNFRep -> (CNFRep, [Int])
propUnits 
  = undefined

-- 4 marks
dp :: CNFRep -> [[Int]]
dp 
  = undefined

--------------------------------------------------------------------------
-- Part IV

-- Bonus 2 marks
allSat :: Formula -> [[(Id, Bool)]]
allSat
  = undefined


