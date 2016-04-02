{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Expression ( Expression(..)
                , Expression_(..)
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
                , Constant(..)
                , Add(..)
                , Sub(..)
                , Mul(..)
                , Div(..)
                , Function(..)
) where


class Expression_ e where
    eval :: e -> [Double] -> Double
data Expression = forall e. (Expression_ e) => Expression e
instance Expression_ Expression where
    eval (Expression e) = eval e

data Component = Component Int
    deriving (Show, Eq)
component :: Int -> Expression
component = Expression . Component

x0, x1, x2, x3, x4, x5, x6, x7, x8, x9 :: Expression
x0 = component 0
x1 = component 1
x2 = component 2
x3 = component 3
x4 = component 4
x5 = component 5
x6 = component 6
x7 = component 7
x8 = component 8
x9 = component 9
x :: Int -> Expression
x = component


data Constant = Constant Double
    deriving (Show, Eq)
constant :: Double -> Expression
constant = Expression . Constant

-- TODO negate

data Add a b = Add a b
    deriving (Show, Eq)
infixl 6 .+
(.+) :: (Expression_ e, Expression_ e') => e -> e' -> Expression
a .+ b = Expression $ Add a b
infixl 6 :+:
type a :+: b = Add a b -- TODO (:+:) = Add ??

data Sub a b = Sub a b
    deriving (Show, Eq)
infixl 6 .-
(.-) :: (Expression_ e, Expression_ e') => e -> e' -> Expression
a .- b = Expression $ Sub a b
infixl 6 :-:
type a :-: b = Sub a b

data Mul a b = Mul a b
    deriving (Show, Eq)
infixl 7 .*
(.*) :: (Expression_ e, Expression_ e') => e -> e' -> Expression
a .* b = Expression $ Mul a b
infixl 7 :*:
type a :*: b = Mul a b

data Div a b = Div a b
    deriving (Show, Eq)
infixl 7 ./
(./) :: (Expression_ e, Expression_ e') => e -> e' -> Expression
a ./ b = Expression $ Div a b
infixl 7 :/:
type a :/: b = Div a b

data Function a = Function (Double -> Double) a
infixr 0 .$
(.$) :: (Expression_ e) => (Double -> Double) -> e -> Expression
f .$ a = Expression $ Function f a
infixr 0 :$:
type f :$: a = Function a

-- TODO Remove the repeated code here with something like (*) <$> a <*> b (and make Expression_ applicative or whutever)

instance (Expression_ a, Expression_ b) => Expression_ (Add a b) where
    eval (Add a b) rows = (+) evalA evalB
        where evalA = eval a rows
              evalB = eval b rows

instance (Expression_ a, Expression_ b) => Expression_ (Sub a b) where
    eval (Sub a b) rows = (-) evalA evalB
        where evalA = eval a rows
              evalB = eval b rows

instance (Expression_ a, Expression_ b) => Expression_ (Mul a b) where
    eval (Mul a b) rows = (*) evalA evalB
        where evalA = eval a rows
              evalB = eval b rows

instance (Expression_ a, Expression_ b) => Expression_ (Div a b) where
    eval (Div a b) rows = (/) evalA evalB
        where evalA = eval a rows
              evalB = eval b rows

instance (Expression_ a) => Expression_ (Function a) where
    eval (Function f a) rows = f $ eval a rows

instance Expression_ Constant where
    eval (Constant f) = const f
instance Expression_ Double where
    eval f = const f

instance Expression_ Component where
    eval (Component n) rows = rows !! n
