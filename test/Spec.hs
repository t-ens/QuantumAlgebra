import Test.Hspec
import Test.QuickCheck
import FreeLie

instance Arbitrary LW where
  arbitrary = do
    n <- choose (1::Int,4)
    m <- choose (2::Int, 4)
    let words = allLyndonWords n [1,2..m]
    index <- choose (0, (length words) - 1 )
    return (words!!index)

instance (Arbitrary a, Arbitrary b) => Arbitrary (FreeVec a b) where
  arbitrary = do 
    k <- choose (0::Int,5)
    genLC <- sequence [ arbitrary | _ <- [1..k] ]
    return $ LC genLC


--Abelian Group Axioms
agAssocLaw :: (AbelianGroup a, Eq a) => a-> a-> a -> Bool
agAssocLaw x y z = (x `agAdd` (y `agAdd` z)) == ((x `agAdd` y) `agAdd` z)

agUnitLaw :: (AbelianGroup a, Eq a) => a -> Bool
agUnitLaw x = and [(agId `agAdd` x) == x, (x `agAdd` agId) == x]

agAbelianLaw :: (AbelianGroup a, Eq a) => a -> a -> Bool
agAbelianLaw x y = (x `agAdd` y) == (y `agAdd` x)

agInverseLaw :: (AbelianGroup a, Eq a) => a -> Bool
agInverseLaw x = (x `agAdd` (agInv x)) == agId

--Vector space scalar multiplication axioms
vsGroupDistLaw :: (Eq b, VectorSpace a b) => a -> b -> b -> Bool
vsGroupDistLaw c x y = c `vScal` ( x `agAdd` y) == 
  (c `vScal` x) `agAdd` (c `vScal` y)

vsFieldDistLaw :: (Eq b, VectorSpace a b) => a -> a -> b -> Bool
vsFieldDistLaw c d x = (c `fAdd` d) `vScal` x == 
  (c `vScal` x) `agAdd` (d `vScal` x)

vsScalarAssoc :: (Eq b, VectorSpace a b) => a -> a -> b -> Bool
vsScalarAssoc c d x = c `vScal` (d `vScal` x) == 
  (c `fMult` d ) `vScal` x

vsUnitLaw :: (Eq b, VectorSpace a b) => a -> b -> Bool
vsUnitLaw c x = (c `fMult` (fInv c) ) `vScal` x == x --Fix types so I can use fAId


--FIXME: Add lie algebra class and define these tests generally
--Lie algebra bracket axioms

lieAlgLinearOne :: Rational -> DKS -> DKS -> DKS -> Bool
lieAlgLinearOne a x y z = bracket ((a `vScal` x) `agAdd` y) z ==
  a `vScal` bracket x z `agAdd` bracket y z

lieAlgLinearTwo :: Rational -> DKS -> DKS -> DKS -> Bool
lieAlgLinearTwo a x y z = bracket z ((a `vScal` x) `agAdd` y)  ==
  a `vScal` bracket z x `agAdd` bracket z y

lieAlgASymm :: DKS -> DKS -> Bool 
lieAlgASymm x y = bracket x y == (fNeg (fMId::Rational)) `vScal` bracket y x 

lieAlgJacobi :: DKS -> DKS -> DKS -> Bool
lieAlgJacobi x y z =
  bracket x (bracket y z) `agAdd` bracket y (bracket z x)
    `agAdd` bracket z (bracket x y) == agId

main :: IO ()
main = hspec $ do 
  describe "DKS is an abelian group under addition:" $ do

    it "Addition is abelian" $ do
      property $ (agAbelianLaw :: DKS -> DKS -> Bool)

    it "Addition is associative" $ do
      property $ (agAssocLaw :: DKS -> DKS -> DKS -> Bool) 

    it "Unit laws hold for addition" $ do
      property $ (agUnitLaw :: DKS -> Bool) 
    
    it "Additive inverse laws hold" $ do
      property $ (agInverseLaw :: DKS -> Bool) 

  describe "DKS is a vector space:" $ do
    
    it "Scalar multiplication distributes over group addition" $ do
      property $ (vsGroupDistLaw :: Rational -> DKS -> DKS -> Bool)

    it "Scalar multiplication distributes over field addition" $ do
      property $ (vsFieldDistLaw :: Rational -> Rational -> DKS -> Bool)

    it "Scalar multiplication associates:" $ do
      property $ (vsScalarAssoc :: Rational -> Rational -> DKS -> Bool)
    
    it "Scalar multiplication unit law holds:" $ do
      property $ (vsUnitLaw :: Rational -> DKS -> Bool)

  describe "DKS is a Lie algebra:" $ do

    it "The bracket is linear in the first argument:" $ do
      property $ lieAlgLinearOne

    it "The bracket is linear in the second argument:" $ do
      property $ lieAlgLinearTwo

    it "The bracket is antisymmetric:" $ do
      property $ lieAlgASymm

    it "The bracket satisfies the Jacobi Identity" $
      property $ (lieAlgJacobi :: DKS -> DKS -> DKS -> Bool)
    

