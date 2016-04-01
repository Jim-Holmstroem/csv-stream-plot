{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

class Var v where
    grab :: v -> [Double] -> Double

data Component = Component Int
    deriving (Show, Eq)

data Constant = Constant Double
    deriving (Show, Eq)

data Add a b = Add a b
    deriving (Show, Eq)
-- TODO Sub/Neg possible?
data Mul a b = Mul a b
    deriving (Show, Eq)
data Div a b = Div a b
    deriving (Show, Eq)

type a + b = Add a b
type a * b = Mul a b  -- TODO * has lower binding strength than
type a / b = Div a b


a .+ b = Add a b
a .* b = Mul a b
a ./ b = Div a b


instance (Var a, Var b) => Var (Add a b) where
    grab (Add a b) rows = grabA + grabB
        where grabA = grab a rows
              grabB = grab b rows

instance (Var a, Var b) => Var (Mul a b) where
    grab (Mul a b) rows = grabA * grabB
        where grabA = grab a rows  -- TODO Remove the repeated code here with something like (*) <$> a b (and make Variable applicative or whutever)
              grabB = grab b rows

instance (Var a, Var b) => Var (Div a b) where
    grab (Div a b) rows = grabA / grabB
        where grabA = grab a rows
              grabB = grab b rows

instance Var Constant where
    grab (Constant f) _ = f

instance Var Component where
    grab (Component n) rows = rows !! n


test :: Component + (Component * Component)
test = Add (Component 2) (Mul (Component 0) (Component 1))
