{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Expression ( Expr(..)
                , expr
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

data Evaluable = forall e . (Expr e) => MkEvaluable e
expr :: (Expr e) => e -> Evaluable
expr = MkEvaluable

class Expr e where
    eval :: e -> [Double] -> Double

data Component = Component Int
    deriving (Show, Eq)

x0, x1, x2, x3, x4, x5, x6, x7, x8, x9 :: Component
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


instance (Expr a, Expr b) => Expr (Add a b) where
    eval (Add a b) rows = (+) grabA grabB
        where grabA = eval a rows
              grabB = eval b rows

instance (Expr a, Expr b) => Expr (Sub a b) where
    eval (Sub a b) rows = (-) grabA grabB
        where grabA = eval a rows
              grabB = eval b rows

instance (Expr a, Expr b) => Expr (Mul a b) where
    eval (Mul a b) rows = (*) grabA grabB
        where grabA = eval a rows  -- TODO Remove the repeated code here with something like (*) <$> a b (and make Expr applicative or whutever)
              grabB = eval b rows

instance (Expr a, Expr b) => Expr (Div a b) where
    eval (Div a b) rows = (/) grabA grabB
        where grabA = eval a rows
              grabB = eval b rows

instance (Expr a) => Expr (Function a) where
    eval (Function f a) rows = f $ eval a rows

instance Expr Constant where
    eval (Constant f) _ = f
instance Expr Double where
    eval f _ = f

instance Expr Component where
    eval (Component n) rows = rows !! n


