{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

module Variable ( Var(..)
                , x0
                , x1
                , x2
                , x3
                , x4
                , x5
                , x6
                , x7
                , x8
                , x9
                , x
                , (.+)
                , (.-)
                , (.*)
                , (./)
                , (.$)
                , (:+:)
                , (:-:)
                , (:*:)
                , (:/:)
                , (:$:)
                , Component(..)
                , componentId
                , Constant(..)
                , Add(..)
                , Sub(..)
                , Mul(..)
                , Div(..)
                , Function(..)
) where

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
x :: Int -> Component
x = Component

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

data Function a = Function (Double -> Double) a

infixl 6 :+:
type a :+: b = Add a b
infixl 6 :-:
type a :-: b = Sub a b
infixl 7 :*:
type a :*: b = Mul a b  -- TODO * has lower binding strength than
infixl 7 :/:
type a :/: b = Div a b
infixr 0 :$:
type f :$: a = a  -- A bit weird but here for completness purposes


-- TODO check what operator order of these
infixl 6 .+
a .+ b = Add a b
infixl 6 .-
a .- b = Sub a b
infixl 7 .*
a .* b = Mul a b
infixl 7 ./
a ./ b = Div a b

infixr 0 .$
f .$ a = Function f a

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

instance (Var a) => Var (Function a) where
    grab (Function f a) rows = f $ grab a rows

instance Var Constant where
    grab (Constant f) _ = f
instance Var Double where
    grab f _ = f

instance Var Component where
    grab (Component n) rows = rows !! n
