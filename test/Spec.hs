import Test.Hspec
import Test.QuickCheck
import FreeLie

instance Arbitrary LW where
  arbitrary = do
    n <- choose (1::Int,5)
    m <- choose (2::Int, 4)
    let words = allLyndonWords n [1,2..m]
    index <- choose (0, (length words) - 1 )
    return (words!!index)

instance (Arbitrary a, Arbitrary b) => Arbitrary (FreeVec a b) where
  arbitrary = do 
    k <- choose (0::Int,5)
    genLC <- sequence [ arbitrary | _ <- [1..k] ]
    return $ LC genLC

agAssocLaw :: (AbelianGroup a, Eq a) => a-> a-> a -> Bool
agAssocLaw x y z = (x `agAdd` (y `agAdd` z)) == ((x `agAdd` y) `agAdd` z)

agUnitLaw :: (AbelianGroup a, Eq a) => a -> Bool
agUnitLaw x = and [(agId `agAdd` x) == x, (x `agAdd` agId) == x]

--agUnitLaw :: (FreeVec Rational LW) -> Bool
--agUnitLaw x = and [ simplify (agId `agAdd` x) == simplify x, simplify (x `agAdd` agId) == simplify x]

agAbelianLaw :: (AbelianGroup a, Eq a) => a -> a -> Bool
agAbelianLaw x y = (x `agAdd` y) == (y `agAdd` x)

agInverseLaw :: (AbelianGroup a, Eq a) => a -> Bool
agInverseLaw x = (x `agAdd` (agInv x)) == agId

lieAlgebraJacobi :: (FreeVec Rational LW) -> (FreeVec Rational LW)
  -> (FreeVec Rational LW) -> Bool
lieAlgebraJacobi x y z =
  bracket x (bracket y z) `agAdd` bracket y (bracket z x)
    `agAdd` bracket z (bracket x y) == (agId::(FreeVec Rational LW))

main :: IO ()
main = hspec $ do 
  describe "Verifying mathematical structures satify defining axioms:" $ do

    it "FreeVec Rational LW is abelian" $ do
      property $ (agAbelianLaw :: (FreeVec Rational LW) -> (FreeVec Rational LW) -> Bool)

    it "FreeVec Rational LW safies additive associativity" $ do
      property $ (agAssocLaw :: (FreeVec Rational LW) -> (FreeVec Rational LW) 
        -> (FreeVec Rational LW) -> Bool) 

    it "FreeVec Rational LW safies additive unit laws" $ do
      property $ (agUnitLaw :: (FreeVec Rational LW) -> Bool) 
    
    it "FreeVec Rational LW safies additive inverse laws" $ do
      property $ (agInverseLaw :: (FreeVec Rational LW) -> Bool) 

    it "FreeVec Rational LW satisfies Jacobi Identity" $
      property $ lieAlgebraJacobi
    

