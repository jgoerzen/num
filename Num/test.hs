{-
Copyright (c) 2005 John Goerzen
-}


module NumTest where
import MissingH.Str

data Op = Plus | Minus | Mul | Div | Pow
        deriving (Eq, Show)

data SymbolicManip = Number Double
                   | Symbol String
                   | BinaryArith Op SymbolicManip SymbolicManip
                   | UnaryArith String SymbolicManip
      deriving(Eq, Show)
                     

instance Num SymbolicManip where
    a + b = BinaryArith Plus a b
    a - b = BinaryArith Minus a b
    a * b = BinaryArith Mul a b
    negate a = BinaryArith Mul (Number (-1)) a
    abs a = UnaryArith "abs" a
    signum _ = error "signum is unimplemented"
    fromInteger i = Number (fromInteger i)

instance Fractional SymbolicManip where
    a / b = BinaryArith Div a b
    recip a = BinaryArith Div (Number 1) a
    fromRational r = Number (fromRational r)

instance Floating SymbolicManip where
    pi = Symbol "pi"
    exp a = UnaryArith "exp" a
    log a = UnaryArith "log" a
    sqrt a = UnaryArith "sqrt" a
    a ** b = BinaryArith Pow a b
    sin a = UnaryArith "sin" a
    cos a = UnaryArith "cos" a
    tan a = UnaryArith "tan" a
    asin a = UnaryArith "asin" a
    acos a = UnaryArith "acos" a
    atan a = UnaryArith "atan" a
    sinh a = UnaryArith "sinh" a
    cosh a = UnaryArith "cosh" a
    tanh a = UnaryArith "tanh" a
    asinh a = UnaryArith "asinh" a
    acosh a = UnaryArith "acosh" a
    atanh a = UnaryArith "atanh" a

data Units = Units Double SymbolicManip
           deriving (Eq)
instance Num Units where
    (Units xa ua) + (Units xb ub) 
        | ua == ub = Units (xa + xb) ua
        | otherwise = error "Mis-matched units in add"
    (Units xa ua) - (Units xb ub) = (Units xa ua) + (Units (xb * (-1)) ub)
    (Units xa ua) * (Units xb ub) = Units (xa * xb) (ua * ub)
    negate (Units xa ua) = Units (negate xa) ua
    abs (Units xa ua) = Units (abs xa) ua
    signum (Units xa _) = Units (signum xa) (Number 1)
    fromInteger i = Units (fromInteger i) (Number 1)

instance Fractional Units where
    (Units xa ua) / (Units xb ub) = Units (xa / xb) (ua / ub)
    recip a = 1 / a
    fromRational r = Units (fromRational r) (Number 1)

instance Floating Units where
    pi = (Units pi (Number 1))
    exp _ = error "exp not yet implemented in Units"
    log _ = error "log not yet implemented in Units"
    (Units xa ua) ** (Units xb ub) 
        | ub == Number 1 = Units (xa ** xb) (ua ** Number xb)
        | otherwise = error "units for RHS of ** not supported"
    sqrt (Units xa ua) = Units (sqrt xa) (sqrt ua)
    sin (Units xa ua) 
        | ua == Symbol "rad" = Units (sin xa) (Number 1)
        | ua == Symbol "deg" = Units (sin (deg2rad xa)) (Number 1)
        | otherwise = error "Units for sin must be deg or rad"
    cos (Units xa ua) 
        | ua == Symbol "rad" = Units (cos xa) (Number 1)
        | ua == Symbol "deg" = Units (cos (deg2rad xa)) (Number 1)
        | otherwise = error "Units for cos must be deg or rad"
    tan (Units xa ua)
        | ua == Symbol "rad" = Units (tan xa) (Number 1)
        | ua == Symbol "deg" = Units (tan (deg2rad xa)) (Number 1)
        | otherwise = error "Units for tan must be deg or rad"
    asin (Units xa ua) 
        | ua == Number 1 = Units (rad2deg $ asin xa) (Symbol "deg")
        | otherwise = error "Units for asin must be empty"
    acos (Units xa ua)
        | ua == Number 1 = Units (rad2deg $ acos xa) (Symbol "deg")
        | otherwise = error "Units for acos must be empty"
    atan (Units xa ua)
        | ua == Number 1 = Units (rad2deg $ atan xa) (Symbol "deg")
        | otherwise = error "Units for atan must be empty"
    sinh = error "sinh not yet implemented in Units"
    cosh = error "cosh not yet implemented in Units"
    tanh = error "tanh not yet implemented in Units"
    asinh = error "asinh not yet implemented in Units"
    acosh = error "acosh not yet implemented in Units"
    atanh = error "atanh not yet implemented in Units"

units :: Double -> String -> Units
units a b = Units a (Symbol b)
                                                    
instance Show Units where
    show (Units xa ua) = show xa ++ "_" ++ prettyShow (simplify ua)

    
deg2rad x = 2 * pi * x / 360
rad2deg x = 360 * x / (2 * pi)

op2str :: Op -> String
op2str Plus = "+"
op2str Minus = "-"
op2str Mul = "*"
op2str Div = "/"
op2str Pow = "**"

prettyShow :: SymbolicManip -> String
prettyShow (Number x) = show x
prettyShow (Symbol x) = x
prettyShow (BinaryArith op a b) =
    let pa = simpleParen a
        pb = simpleParen b
        pop = op2str op
        in pa ++ pop ++ pb
prettyShow (UnaryArith op a) = 
    op ++ "(" ++ show a ++ ")"

rpnShow :: SymbolicManip -> String
rpnShow i =
    let toList (Number x) = [show x]
        toList (Symbol x) = [x]
        toList (BinaryArith op a b) = toList a ++ toList b ++
           [op2str op]
        toList (UnaryArith op a) = op : toList a
    in join " " (toList i)

texShow :: SymbolicManip -> String

simpleParen (Number x) = prettyShow (Number x)
simpleParen (Symbol x) = prettyShow (Symbol x)
simpleParen (BinaryArith op a b) = "(" ++ prettyShow (BinaryArith op a b) ++ ")"
simpleParen (UnaryArith op a) = prettyShow (UnaryArith op a)

simplify :: SymbolicManip -> SymbolicManip
simplify (BinaryArith Mul (Number 1) b) = b
simplify (BinaryArith Mul a (Number 1)) = a
simplify (BinaryArith Div a (Number 1)) = a
simplify (BinaryArith Plus (Number 0) b) = b
simplify (BinaryArith Plus a (Number 0)) = a
simplify (BinaryArith Minus a (Number 0)) = a
simplify x = x

test :: forall a. (Num a) => a
test = 2 * 5 + 3