{-
Copyright (c) 2005 John Goerzen
-}


module NumTest where

data Op = Plus | Minus | Mul | Div | Abs | Pow
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
    signum a = error "signum is unimplemented"
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
