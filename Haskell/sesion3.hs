-- Daniel Alfaro Miranda

-- EJERCICIO 1
--a
listaOscilante :: Integral a => [a]
listaOscilante = [x*y | x <- [1..], y <- [1, -1]]
listaOscilante' = concat $ map (\x -> map (\y -> x*y) [1, -1]) [1..]

--b
parejasNaturales :: Integral a => [(a,a)]
parejasNaturales = [(y,z) | x <- [0..], y <- [0..x], z <- [0..x], z+y == x]
parejasNaturales' = concat $ map (\x -> concat $ map (\y -> map (\z -> (y,z)) $ filter (\z -> z+y == x) [0..x])  [0..x]) [0..]

-- EJERCICIO 2
--a
sufijos :: [a] -> [[a]]
sufijos xs = reverse $ foldr (\x acc -> (x:(acc!!0)):acc) [[]] xs
sufijos' :: [a] -> [[a]]
sufijos' xs = [drop ((length xs) - n) xs | n <- [0..length xs]]

--b
sublists :: [a] -> [[a]]
sublists xs = [take n . drop i $ xs | n <- [1..length xs], i <- [0..((length xs) - 1)], length(drop i $ xs) >= n]

--c
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat [intercala x ys | ys <- perms xs]

intercala :: a -> [a] -> [[a]]
intercala x [] = [[x]]
intercala x (y:ys) = (x:y:ys) : [ y:zs | zs <- intercala x ys]

--d
sumandos :: Integral a => a -> [[a]]
sumandos n
      | n <= 0 = error "Solo positivos"
      | otherwise = sumandos'' n 0 1

sumandos' :: Integral a => a -> a -> [[a]]
sumandos' n x
      | x == n = [[]] 
      | otherwise = [z:zs | z <- [y | y <- [1..n], y + x <= n], zs <- sumandos' n (z+x)] 

sumandos'' :: Integral a => a -> a -> a -> [[a]]
sumandos'' n x i
      | x == n = [[]] 
      | otherwise = [z:zs | z <- [y | y <- [1..n], y + x <= n, y >= i], zs <- sumandos'' n (z+x) z] 