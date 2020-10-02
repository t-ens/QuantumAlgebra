{-# LANGUAGE MultiParamTypeClasses #-} --Needed for field
{-# LANGUAGE AllowAmbiguousTypes #-}   --Needed for field
{-# Language FlexibleInstances #-}     --Needed to make Rational instances

module FreeLie
    ( someFunc
    ) where

import Data.List

--Basic mathematical structures

class Field a where
  fAdd :: a -> a -> a
  fMult :: a -> a -> a
  fNeg :: a -> a
  fInv :: a -> Maybe a --inversion is only partially defined 
  fAId :: a
  fMId :: a

class Field a => VectorSpace a b where
  vAdd :: b -> b -> b
  vScal :: a -> b -> b

class VectorSpace a b => LieAlgebra a b where
  lBracket :: b -> b -> b


-- Implementation of Free Lie algebras over the rationals

instance Field Rational where
  fAdd a b = a+b
  fNeg a = (-1)*a
  fMult a b = a*b
  fInv 0 = Nothing
  fInv a = Just (1/a)
  fAId = 0
  fMId = 1    

--The free vector space with basis b over the field is a list of scalar
--multiples of basis elements(=
type FreeVec a b = [(a,b)]   

--collect terms
collect :: (Field a, Eq b) => [(a,b)] -> [(a,b)]
collect [] = []
collect ((x,y):xs) = (sumFirst (x,y) match) : (collect non)
  where 
    (match, non) = partition (((==) y).snd) xs
    sumFirst a ys = foldr (\s t -> ( (fst s) `fAdd` (fst t), snd s)) a ys

instance (Field a, Eq b) => VectorSpace a (FreeVec a b) where
  vAdd v1 v2 = collect.concat $ [v1, v2] 
  vScal f v = map (\(a,b) -> (f `fMult` a,b)) v 



-- Lyndon Words are a basis of the rational Free Lie algebra
-- A Lyndon word is lexicographically smaller than all of its proper right
-- factors
newtype LW = LW {theWord :: [Int]} deriving (Eq,Ord,Show)

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

bracket :: (FreeVec Rational LW) -> (FreeVec Rational LW) -> (FreeVec Rational LW)
bracket x y = x

lwAdjoint :: LW -> LW -> FreeVec Rational LW 
lwAdjoint w = (\z ->
  if w==z
    then [(0,w)]
    else if z < w
      then (fNeg fMId) `vScal` (bracket [(1,z)] [(1,w)])
      else if (deg w == 1)
        then [(1,LW $ (theWord w) ++ (theWord z))]
        else
          [(fAId,fAId)]
  )

someFunc :: IO ()
someFunc = putStrLn "someFunc"
