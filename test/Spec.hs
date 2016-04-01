import Variable

import Test.QuickCheck

-- TODO how do you split it up inte different test for different modules (within the same test suite)?

instance Arbitrary Constant where
    arbitrary = Constant <$> arbitrary
instance Arbitrary Component where
    arbitrary = Component <$> abs <$> arbitrary
instance (Arbitrary a, Arbitrary b) => Arbitrary (Add a b) where
    arbitrary = Add <$> arbitrary <*> arbitrary
instance (Arbitrary a, Arbitrary b) => Arbitrary (Sub a b) where
    arbitrary = Sub <$> arbitrary <*> arbitrary
instance (Arbitrary a, Arbitrary b) => Arbitrary (Mul a b) where
    arbitrary = Mul <$> arbitrary <*> arbitrary
instance (Arbitrary a, Arbitrary b) => Arbitrary (Div a b) where
    arbitrary = Div <$> arbitrary <*> arbitrary




-- RangeList is a list of at least length n and on the form [0.0,1.0,...]
data RangeList = RangeList [Double]
    deriving (Show, Eq)
data RowVarPair v = RowVarPair RangeList v
    deriving (Show, Eq)
--instance (Var v) => Arbitrary (RowVarPair v) where
--    arbitrary = do
--        NonNegative n <- arbitrary
--        RowVarPair <$> (rangeList <$> n) <*> arbitrary


rangeList :: Int -> [Double]
rangeList n = fromIntegral <$> [0..n]

prop_grabConstant :: Double -> [Double] -> Bool
prop_grabConstant c rows = grab (Constant c) rows == c

prop_grabComponent :: NonNegative Int -> NonNegative Int -> Bool
prop_grabComponent (NonNegative n) (NonNegative m) = grab (Component n) rows == fromIntegral n
    where rows = rangeList (n+m)

--prop_grabAdd :: (Var a, Var b) => Add a b -> NonNegative Int -> NonNegative Int -> Bool
--prop_grabAdd add@(Add a b) (NonNegative n) (NonNegative m) = (grab add rows) == (grab a rows) + (grab b rows)
--    where rows = rangeList (n+m)
--



