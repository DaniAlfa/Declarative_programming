data Pila a = P [a] deriving (Show)

creaPila :: Pila a
creaPila = P []

apilar :: a -> Pila a -> Pila a
apilar x (P ys) = P (x:ys)

esPilaVacia :: Pila a -> Bool
esPilaVacia (P []) = True
esPilaVacia (P _) = False

desapilar :: Pila a -> Pila a
desapilar (P (x:xs)) = P xs

cima :: Pila a -> a
cima (P (x:xs)) = x

r :: [a] -> [a]
r xs = ys
    where P ys = foldl (\p x -> apilar x p) creaPila xs

primeroQueCumple :: (a -> Bool) ->[a] -> Maybe a
primeroQueCumple p [] = Nothing
primeroQueCumple p (x:xs) 
                | p x = Just x
                | otherwise = primeroQueCumple p xs

data Cj a = C [a] deriving (Show)

cjVacio :: Cj a
cjVacio = C []

cjIsEmpty :: Cj a -> Bool
cjIsEmpty (C []) = True
cjIsEmpty (C (x:xs)) = False

pertenece :: (Eq a) => Cj a -> a -> Bool
pertenece (C ys) x = elem x ys

incluyeElem :: (Eq a) => [a] -> a -> [a]
incluyeElem xs e 
    | pertenece (C xs) e = xs
    | otherwise = e:xs

quitaRepeticiones ::Eq a => Cj a -> Cj a
quitaRepeticiones (C xs) = C (foldl incluyeElem [] xs)

cjElements :: Cj a -> [a]
cjElements (C ys) = ys

add :: (Eq a) => a -> Cj a -> Cj a
add x (C ys) = if (pertenece (C ys) x) then (C ys) else (C (x:ys))

--contenido :: Cj a -> Cj a -> Bool
--contenido (C xs) (C ys)

--instance Eq Cj a where
   -- c1 == c2 = contenido c1 c2 && contenido c2 c1

--instance Ord Cj a where
   -- c1 <= c2 = contenido c1 c2

data Mat = Mat [[Float]] deriving (Show)

--type Vector = [Float]
--type Matriz = [Vector]

ejMat = Mat [[2,4],[5,6],[8,9]]

dimensionesMat :: Mat -> (Int, Int)
dimensionesMat (Mat []) = (0, 0)
dimensionesMat (Mat (x:xs)) = (1 + length xs, length x)

transpuesta :: Mat -> Mat
transpuesta (Mat []) = Mat []
transpuesta (Mat ys) = Mat [ [xs !! i | xs <- ys] | i <- [0..(snd (dimensionesMat (Mat ys)))-1]]
--hacerlo con recursion
--transpuesta (Col ys) = Col (transpuestaAux (Col ys) (snd (dimensionesMat (Col ys))))

--transpuestaAux :: Mat -> Int -> [Mat]
--transpuestaAux (Col ys) n = [ Fila [xs !! i | (Fila xs) <- ys] | i <- [0..n-1]]

data Temp = Kelvin Float | Celsius Float | Fahrenheit Float deriving (Show)
ceroAbs = -273.15

toCelsius :: Temp -> Temp
toCelsius c = c
toCelsius (Kelvin x) = Celsius (x + ceroAbs)
toCelsius (Fahrenheit x) = Celsius ( )

instance Eq Temp where
    c1 =

