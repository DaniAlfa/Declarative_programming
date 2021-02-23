--Daniel Alfaro Miranda
--Ismail Azizi Gonzalez

--1)
data Nat = Cero | Suc Nat deriving (Eq, Ord)

infixr 6 +.
(+.) :: Nat -> Nat -> Nat
Cero +. x = x
Suc(x) +. y = Suc(x +. y) 

infixr 7 *.
(*.) :: Nat -> Nat -> Nat
Cero *. _ = Cero
Suc(x) *. y = x *. y +. y

natToInt :: Nat -> Int
natToInt Cero = 0
natToInt (Suc x) = 1 + natToInt x

instance Show Nat where
    --show :: Nat -> [Char]
    show x = show(natToInt x) 

--2)
data Complex = C Float Float deriving (Eq)

instance Show Complex where
    show (C x y) = show x ++ f y
        where f y 
                | y >= 0 = "+" ++ show y ++ "i"
                | otherwise = show y ++ "i"

instance Num Complex where
    (C a b) + (C c d) = C (a+c) (b+d)
    (C a b) * (C c d) = C (a*c - b*d) (b*c + a*d)
    (C a b) - (C c d) = C (a-c) (b-d)

--3)
class Medible a where
    tamanyo :: a -> Int

instance Medible Bool where
    tamanyo False = 0
    tamanyo True = 1

instance Medible a => Medible [a] where
    tamanyo [] = 0
    tamanyo (x:xs) = tamanyo x + tamanyo xs

instance (Medible a, Medible b) => Medible (a,b) where
    tamanyo (x,y) = tamanyo x + (tamanyo y)
 

instance Medible Int where
    tamanyo x
        | x < 0 = -1
        | x == 0 = 0
        | otherwise = 1 