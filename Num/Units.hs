-- Copyright (c) 2005 John Goerzen

module Units where

class Unit a where 
    unit ::  Double -> a
    value :: a -> Double

newtype Meters = Meters Double
    deriving (Eq, Show)
newtype Seconds = Seconds Double
    deriving (Eq, Show)

instance Unit Meters where
    unit = Meters
    value (Meters x) = x
instance Unit Seconds where
    unit = Seconds
    value (Seconds x) = x

newtype Prod a b = Prod Double
    deriving (Eq, Show)

instance (Unit a, Unit b) => Unit (Prod a b) where
    unit = Prod
    value (Prod x) = x

--infix 7 *$
infix 6 +$

(+$) :: (Unit a) => a -> a -> a
x +$ y = unit (value x + value y)
(*$) :: (Unit a, Unit b) => a -> b -> Prod a b
x *$ y = Prod (value x * value y)