-- Copyright (c) 2005 John Goerzen

module Units where

class FundamentalUnit a where
    uName :: a -> String
    uAbbr :: a -> String
    
    uName = uAbbr

data FUEncap = forall a. FundamentalUnit a => FUEncap a
type Units = [FUEncap]

data Meter = Meter
             deriving (Eq, Show)

instance FundamentalUnit Meter where
    uAbbr _ = "m"

data (Num a) => UnitNum a = UnitNum a Units
{-
instance (Num a) => Num (UnitNum a) where
    (UnitNum xa ua) + (UnitNum xa ub)
-}