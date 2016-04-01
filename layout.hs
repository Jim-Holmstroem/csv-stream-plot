{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

import Test.QuickCheck

-- TODO Phantom Typing to ensure that if it compiles with n cols and we get n rows grab will always work :)

class Var v where
    grab :: v -> [Double] -> Double

data Component = Component Int
    deriving (Show, Eq)

x0, x1, x2, x3 :: Component
x0 = Component 0
x1 = Component 1
x2 = Component 2
x3 = Component 3
x4 = Component 4
x5 = Component 5
x6 = Component 6
x7 = Component 7
x8 = Component 8
x9 = Component 9

componentId :: Component -> Int
componentId (Component n) = n

data Constant = Constant Double
    deriving (Show, Eq)



data Add a b = Add a b
    deriving (Show, Eq)
-- TODO Sub/Neg possible?
data Sub a b = Sub a b
    deriving (Show, Eq)
data Mul a b = Mul a b
    deriving (Show, Eq)
data Div a b = Div a b
    deriving (Show, Eq)

infixl 6 :+:
type a :+: b = Add a b
infixl 6 :-:
type a :-: b = Sub a b
infixl 7 :*:
type a :*: b = Mul a b  -- TODO * has lower binding strength than
infixl 7 :/:
type a :/: b = Div a b


-- TODO check what operator order of these
infixl 6 .+
a .+ b = Add a b
infixl 6 .-
a .- b = Sub a b
infixl 7 .*
a .* b = Mul a b
infixl 7 ./
a ./ b = Div a b


instance (Var a, Var b) => Var (Add a b) where
    grab (Add a b) rows = (+) grabA grabB
        where grabA = grab a rows
              grabB = grab b rows

instance (Var a, Var b) => Var (Sub a b) where
    grab (Sub a b) rows = (-) grabA grabB
        where grabA = grab a rows
              grabB = grab b rows

instance (Var a, Var b) => Var (Mul a b) where
    grab (Mul a b) rows = (*) grabA grabB
        where grabA = grab a rows  -- TODO Remove the repeated code here with something like (*) <$> a b (and make Variable applicative or whutever)
              grabB = grab b rows

instance (Var a, Var b) => Var (Div a b) where
    grab (Div a b) rows = (/) grabA grabB
        where grabA = grab a rows
              grabB = grab b rows

instance Var Constant where
    grab (Constant f) _ = f
instance Var Double where
    grab f _ = f


instance Var Component where
    grab (Component n) rows = rows !! n


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



