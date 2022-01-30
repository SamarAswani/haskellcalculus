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
  negate      = UnApp Neg
  (+)         = BinApp Add
  (*)         = BinApp Mul
-- Leave the following two undefined...
  signum      = undefined
  abs         = undefined

instance Fractional Exp where
  fromRational = Val . fromRational
  (/)          = BinApp Div
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

lookUp :: Eq a => a -> [(a, b)] -> b
lookUp a b
  = fromJust( lookup a b )

showExp :: Exp -> String
showExp
  = undefined


unAppTable = [(Neg, negate), (Sin, sin), (Cos, cos), (Log, log)]
binAppTable = [(Add, (+)), (Mul, (*)), (Div, (/))]

eval :: Exp -> Env -> Double
eval (Val num) env  
  = num
eval (Id a) env
  = lookUp a env   
eval (UnApp op exp) env
  = lookUp op unAppTable (eval exp env)
eval (BinApp op exp1 exp2) env
  = (lookUp op binAppTable) (eval exp1 env) (eval exp2 env)



diff :: Exp -> String -> Exp
diff (Val a) str
  = 0.0

diff (Id b) str
  | b == str  = 1.0
  | otherwise = 0.0

diff (UnApp Neg exp) str
  = negate (diff exp str)

diff (UnApp Sin exp) str
  = (*) (cos exp) (diff exp str) 

diff (UnApp Cos exp) str
  = negate ((*) (sin exp) (diff exp str))

diff (UnApp Log exp) str
  = (/) (diff exp str) (exp)

diff (BinApp Add exp1 exp2) str
  = (+) (diff exp1 str) (diff exp2 str)

diff (BinApp Mul exp1 exp2) str
  = (+) ((*) (exp1) (diff exp2 str)) ((*) (diff exp1 str) (exp2))        

diff (BinApp Div exp1 exp2) str
  = (/) ((+) ((*) (diff exp1 str) (exp2)) (negate ((*) (exp1) (diff exp2 str)))) ((*) (exp2) (exp2))




maclaurin :: Exp -> Double -> Int -> Double
maclaurin exp num terms
  = sum (zipWith3 (\x y z -> ((eval x [("x",0)]) * (num ^ z)) / y) differentials factorials [0..terms])
    where
      factorials = 1: take terms (scanl1 (*) [1..])
      differentials = take terms (iterate (\x -> diff x "x") exp)
    

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

e7 = UnApp Neg (UnApp Sin (Id "x"))



-- (UnApp Log (BinApp Mul (Val 2.0) (Id "x")))

-- (BinApp Div (BinApp Add (BinApp Mul (Val 2.0) (Val 1.0)) (BinApp Mul (Val 0.0) (Id "x"))) (BinApp Mul (Val 2.0) (Id "x")))
