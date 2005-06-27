-- Copyright (c) 2005 John Goerzen

module Units where

class FundamentalUnit a where
    uName :: a -> String
    uAbbr :: a -> String
    
    uName = uAbbr

data (FundamentalUnit a) => Unit a = Unit [(a, Double)]
    deriving (Eq, Show)

data Meter = Meter
           deriving (Eq, Show)
instance FundamentalUnit Meter where
    uAbbr _ = "m"
