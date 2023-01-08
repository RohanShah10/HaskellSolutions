module SC where

import Data.List
import Data.Maybe

import Types
import Examples

---------------------------------------------------------

prims :: [Id]
prims
  = ["+", "-", "*", "<=", "ite"]

lookUp :: Id -> [(Id, a)] -> a
lookUp v env
  = fromMaybe (error ("lookUp failed with search key " ++ v))
              (lookup v env)

---------------------------------------------------------
-- Part I

isFun :: Exp -> Bool
isFun (Fun lst e) = True
isFun e = False

splitDefs :: [Binding] -> ([Binding], [Binding])
splitDefs [] = ([],[])
splitDefs lst@((i,e) : bs) = splitDefs' lst [] []
    where 
        splitDefs' :: [Binding] -> [Binding] -> [Binding] -> ([Binding], [Binding])
        splitDefs' [] a b = (a,b)
        splitDefs' lst@((i,e): bs) a b 
          |isFun e = splitDefs' (bs) (a++[(i,e)]) (b)
          |otherwise = splitDefs' (bs) (a) (b++[(i,e)])


-- data Exp = Const Int | 
--            Var Id | 
--            Fun [Id] Exp |
--            App Exp [Exp] |
--            Let [Binding] Exp 
--          deriving (Eq, Show)

-- type Id = String

-- type Binding = (Id, Exp)


topLevelFunctions :: Exp -> Int
topLevelFunctions (Let [] e) = 0
topLevelFunctions (Let lst e) = counter' (map snd lst)
    where
        counter' :: [Exp] -> Int 
        counter' es = length (filter (\x -> isFun x) (es))

topLevelFunctions e = 0
---------------------------------------------------------
-- Part II

unionAll :: Eq a => [[a]] -> [a]
unionAll = nub.concat

-- data Exp = Const Int | 
--            Var Id | 
--            Fun [Id] Exp |
--            App Exp [Exp] |
--            Let [Binding] Exp 
--          deriving (Eq, Show)

-- type Id = String

-- type Binding = (Id, Exp)


freeVars :: Exp -> [Id]
freeVars (Const a) = []

freeVars (Var i)
  |elem i prims = []
  |otherwise = [i]

freeVars exp@(App e es) = unionAll (map (freeVars) (e:es))

freeVars (Fun lst es) = freeVars es \\ lst  

freeVars (Let bs e) = unionAll (j) \\ (map fst bs)
    where 
        j = map (freeVars) (map snd bs) ++ [freeVars e]

---------------------------------------------------------
-- Part III

-- Given...
lambdaLift :: Exp -> Exp
lambdaLift e
  = lift (modifyFunctions (buildFVMap e) e)

buildFVMap :: Exp -> [(Id, [Id])]
buildFVMap exp 

modifyFunctions :: [(Id, [Id])] -> Exp -> Exp
-- Pre: The mapping table contains a binding for every function
-- named in the expression.
modifyFunctions 
  = undefined

-- The default definition here is id.
-- If you implement the above two functions but not this one
-- then lambdaLift above will remove all the free variables
-- in functions; it just won't do any lifting.
lift :: Exp -> Exp
lift 
  = id

-- You may wish to use this...
lift' :: Exp -> (Exp, [Supercombinator])
lift' 
  = undefined

prefixes [] = []
prefixes lst =  prefixes (init lst) ++ [lst]
