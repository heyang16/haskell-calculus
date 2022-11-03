module Calculus where

import Data.Maybe

data UnOp = Neg | Sin | Cos | Log
          deriving (Eq, Ord, Show)

data BinOp = Add | Mul | Div
           deriving (Eq, Ord, Show)

data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp
         deriving (Eq, Ord, Show)

type Env = [(String, Double)]

---------------------------------------------------------------------------
-- Type classes and class instances

class Vars a where
  x, y, z :: a

instance Vars Exp where
  x = Id "x"
  y = Id "y"
  z = Id "z"

instance Vars Double where
  x = 4.3
  y = 9.2
  z = -1.7

instance Num Exp where
  fromInteger = Val . fromInteger
  negate (Val 0) = 0
  negate exp = UnApp Neg exp
  (+) (Val 0) exp = exp
  (+) exp (Val 0) = exp
  (+) exp1 exp2 = BinApp Add exp1 exp2
  (*) exp (Val 1) = exp
  (*) (Val 1) exp = exp
  (*) exp1 exp2 = BinApp Mul exp1 exp2
-- Leave the following two undefined...
  signum      = undefined
  abs         = undefined
instance Fractional Exp where
  fromRational = Val . fromRational
  (/) (Val 0) exp = Val 0
  (/) exp (Val 1) = exp
  (/) exp1 exp2 = BinApp Div exp1 exp2
-- Leave the following one undefined...
  recip        = undefined

instance Floating Exp where
  sin     = UnApp Sin
  cos     = UnApp Cos
  log     = UnApp Log
-- Leave the following fifteen undefined...
  tan     = undefined
  asin    = undefined
  acos    = undefined
  atan    = undefined
  pi      = undefined
  exp     = undefined
  sqrt    = undefined
  (**)    = undefined
  logBase = undefined
  sinh    = undefined
  cosh    = undefined
  tanh    = undefined
  asinh   = undefined
  acosh   = undefined
  atanh   = undefined

---------------------------------------------------------------------------
-- Looks up a key in a list of key-value pairs and returns the matching value
-- Pre-condition: key always exists in the list
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp a env = fromJust(lookup a env)

-- Shows an expression in a human readable format
showExp :: Exp -> String
showExp (Val x) = show x
showExp (Id x) = x
showExp (UnApp unOp exp) = unOp' ++ "(" ++ showExp exp ++ ")"
  where
    unOps = [(Neg, "-"), (Sin, "sin"), (Cos, "cos"), (Log, "log")]
    unOp' = lookUp unOp unOps
showExp (BinApp binOp exp1 exp2) = showExp exp1 ++ binOp' ++ showExp exp2
  where
    binOps = [(Add, "+"), (Mul, "*"), (Div, "/")]
    binOp' = lookUp binOp binOps

-- Given an expression and an environment,
-- return the value of the evaluated expression
eval :: Exp -> Env -> Double
eval (Val x) env = x
eval (Id x) env = lookUp x env -- replaces x with its value stated in env
eval (UnApp unOp exp) env = unOp' (eval exp env)
  where
    -- table mapping unary operations to their respective functions
    unOps = [(Neg, negate), (Sin, sin), (Cos, cos), (Log, log)]
    unOp' = lookUp unOp unOps
eval (BinApp binOp exp1 exp2) env = binOp' (eval exp1 env) (eval exp2 env)
  where
    -- table mapping binary operations to their respective functions
    binOps = [(Add, (+)), (Mul, (*)), (Div, (/))]
    binOp' = lookUp binOp binOps

-- Given an expression returns the derivative of that expression
diff :: Exp -> String -> Exp
diff (Val x) var = 0
diff (Id x) var = if x == var then 1 else 0
diff (UnApp unOp exp) var
  | unOp == Neg = negate dx
  | unOp == Sin = cos exp * dx
  | unOp == Cos = negate (sin exp * dx)
  | unOp == Log = dx / exp
  where
    dx = diff exp var
diff (BinApp binOp exp1 exp2) var
  | binOp == Add = dx1 + dx2
  | binOp == Mul = (exp1 * dx2) + (dx1 * exp2)
  | binOp == Div = ((dx1 * exp2) - (exp1 * dx2)) / (exp2 * exp2)
  where
    dx1 = diff exp1 var
    dx2 = diff exp2 var

-- Approximate the value of a function (Exp) at a specified point
-- (Double) using the first n (Int) terms of the Maclaurin series
maclaurin :: Exp -> Double -> Int -> Double
maclaurin exp x n
  = sum fractions
  where
    env = [("x", 0)]
    facs = scanl (*) 1 [1..n-1]
    orders = map (`eval` env) (iterate (`diff` "x") exp)
    powers = iterate (x *) 1
    fractions = zipWith3 (\a b c -> a * b / fromIntegral c) orders powers facs

---------------------------------------------------------------------------
-- Test cases...

e1, e2, e3, e4, e5, e6 :: Exp

-- 5*x
e1 = BinApp Mul (Val 5.0) (Id "x")

-- x*x+y-7
e2 = BinApp Add (BinApp Add (BinApp Mul (Id "x") (Id "x")) (Id "y"))
                (UnApp Neg (Val 7.0))

-- x-y^2/(4*x*y-y^2)::Exp
e3 = BinApp Add (Id "x")
            (UnApp Neg (BinApp Div (BinApp Mul (Id "y") (Id "y"))
            (BinApp Add (BinApp Mul (BinApp Mul (Val 4.0) (Id "x")) (Id "y"))
                        (UnApp Neg (BinApp Mul (Id "y") (Id "y"))))))

-- -cos x::Exp
e4 = UnApp Neg (UnApp Cos (Id "x"))

-- sin (1+log(2*x))::Exp
e5 = UnApp Sin (BinApp Add (Val 1.0)
                           (UnApp Log (BinApp Mul (Val 2.0) (Id "x"))))

-- log(3*x^2+2)::Exp
e6 = UnApp Log (BinApp Add (BinApp Mul (Val 3.0) (BinApp Mul (Id "x") (Id "x")))
                           (Val 2.0))
