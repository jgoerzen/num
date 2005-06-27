-- Copyright (c) 2005 John Goerzen

module Units where

class (Num a) => Unit a b where 
    unit :: a -> b
    value :: b -> a

newtype (Num a) => Meters a = Meters a
    deriving (Eq, Show)
newtype (Num a) => Seconds a = Seconds a
    deriving (Eq, Show)

instance (Num a) => Unit a (Meters a) where
    unit = Meters
    value (Meters x) = x
instance (Num a) => Unit a (Seconds a) where
    unit = Seconds
    value (Seconds x) = x

newtype (Num a) => Prod a b c = Prod a

instance (Num a, Unit a b, Unit a c) => Unit a (Prod a b c) where
    unit = Prod
    value (Prod x) = x

--infix 7 *$
infix 6 +$

(+$) :: (Unit a b) => a b -> a b -> a b
x +$ y = unit (value x + value y)