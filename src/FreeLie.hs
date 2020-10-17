{-# LANGUAGE MultiParamTypeClasses #-} --Needed for field
{-# Language FlexibleInstances #-}     --Needed to make Rational instances

module FreeLie where

import Data.List

--Basic mathematical structures

class Field a where
  fAdd :: a -> a -> a
  fMult :: a -> a -> a
  fNeg :: a -> a
  fInv :: a -> a
  fAId :: a
  fMId :: a

class AbelianGroup a where
  agAdd :: a -> a -> a
  agInv :: a -> a
  agId :: a

class (Field a, AbelianGroup b) => VectorSpace a b where
  vScal :: a -> b -> b

--class VectorSpace a b => LieAlgebra a b where
--  lBracket :: b -> b -> b


-- Implementation of Free Lie algebras over the rationals

instance Field Rational where
  fAdd a b = a+b
  fNeg a = (-1)*a
  fMult a b = a*b
  fInv 0 = 0       --Maybe use maybe instead?
  fInv a = 1/a
  fAId = 0
  fMId = 1    

--The free vector space with basis b over the field is a list of scalar
--multiples of basis elements
data FreeVec a b = LC [(a,b)]

instance (Field a, Eq a, Ord b) => Eq (FreeVec a b) where
  (==) (LC x)  (LC y) = ((simplifyList x) == (simplifyList y))

instance (Show a, Show b) => Show (FreeVec a b) where
  show (LC []) = ""
  show (LC [(x,y)]) = show x ++ "*" ++ show y
  show (LC ((x,y):xs)) = show x ++ "*" ++ show y ++ "+" ++ show (LC xs)

--sort by basis elements for testing equality
basisSort :: Ord b => (a,b) -> (a,b) -> Ordering
basisSort (_,b) (_,c) = compare b c


--collect terms
collectList :: (Field a, Eq b) => [(a,b)] -> [(a,b)]
collectList [] = []
collectList ((x,y):xs) = (sumFirst (x,y) match) : (collectList non)
  where 
    (match, non) = partition (((==) y).snd) xs
    sumFirst a ys = foldr (\s t -> ( (fst s) `fAdd` (fst t), snd s)) a ys
collect :: (Field a, Eq b) => FreeVec a b -> FreeVec a b
collect (LC x) = LC $ collectList x

removeZerosList :: (Field a, Eq a) => [(a,b)] -> [(a,b)]
removeZerosList = filter (((/=) fAId).fst) 
removeZeros (LC x) = LC $ removeZerosList x

simplifyList :: (Field a, Eq a, Ord b) => [(a,b)] -> [(a,b)]
simplifyList = (sortBy basisSort).removeZerosList.collectList
simplify (LC x) = LC $ simplifyList x


instance (Field a, Eq a, Ord b) => AbelianGroup (FreeVec a b) where
  agAdd (LC v1) (LC v2) = LC (simplifyList.concat $ [v1, v2])
  agInv (LC z)  =LC $ map (\(x,y)->(fNeg x,y)) z
  agId = LC []

instance (Field a, Eq a, Ord b) => VectorSpace a (FreeVec a b) where
  vScal f (LC v) = LC $  map (\(c,d) -> (f `fMult` c,d)) v 

-- Lyndon Words are a basis of the rational Free Lie algebra
-- A Lyndon word is lexicographically smaller than all of its proper right
-- factors
newtype LW = LW {theWord :: [Int]} deriving (Eq,Ord)

lyndonQ :: LW -> Bool
lyndonQ  (LW []) = False
lyndonQ (LW w) = foldr (\x -> (&&) ((LW w) < (LW x))) True [drop i w | i <- [1 .. (length w-1)]]

allLyndonWords n alph = filter lyndonQ ( map LW (allWords n alph) )
  where
    allWords 0 alph = [[]]
    allWords n alph = concat [ map ((:) x) (allWords (n-1) alph) | x <- alph ]

deg :: LW -> Int 
deg (LW w) = length w

lyndonFactorization :: LW -> (LW,LW)
lyndonFactorization (LW w)
  | deg (LW w) == 1 = (LW [], LW w)
  | deg (LW w) > 1 = let rf = foldr1 min [ drop i w | i <- [1 .. (length w-1)] ] in
      ( LW $ (reverse.drop (length rf).reverse) w, LW rf )
  | otherwise = (LW [], LW [])

bracketForm :: LW -> String
bracketForm (LW []) = ""
bracketForm (LW [c]) = show c
bracketForm x = "[" ++ (bracketForm a) ++ "," ++ (bracketForm b) ++ "]"
  where (a,b) = lyndonFactorization x

instance Show LW where
  show = bracketForm
  

bracket :: (FreeVec Rational LW) -> (FreeVec Rational LW) -> (FreeVec Rational LW)
bracket (LC xs) (LC ys) = simplify.concatLC $ 
  map (\ ((a,v),(b,w)) -> ((a `fMult` b) `vScal` (lwAdjoint v w))) 
    [ (x,y) | x<-xs, y<-ys ]
      where concatLC :: [FreeVec Rational LW] -> FreeVec Rational LW
            concatLC [] = LC [] 
            concatLC w = foldr1 agAdd w

lwAdjoint :: LW -> LW -> FreeVec Rational LW
lwAdjoint w z = 
  if w == z 
    then LC [(0,w)]
    else if z<w
      then (fNeg fMId::Rational) `vScal` (bracket (LC [(1,z)]) (LC [(1,w)]) )
      else if (deg w == 1)
        then LC [(1, LW $ (theWord w) ++ (theWord z))]
        else
          case (not (y<z)) of
            True -> LC [(1, LW $ (theWord w) ++ (theWord z))]
            False -> (bracket (LC [(fMId::Rational,x)]) (lwAdjoint y z)) `agAdd` 
              (bracket (lwAdjoint x z) (LC [(fMId,y)]))
          where (x,y) = lyndonFactorization w

someFunc :: IO ()
someFunc = putStrLn "someFunc"
