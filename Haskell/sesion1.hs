--Daniel Alfaro

yearsIn10to10Sec = 10^10/60/60/24/365

fyearsIn :: (Real a, Fractional b) => a -> b
fyearsIn a = realToFrac(a)/60/60/24/365


intyear = (truncate years, truncate days, truncate hours, truncate min, truncate seg)
    where years = 10^10/60/60/24/365
          days = (years - fromIntegral(truncate(years))) * 365
          hours = (days - fromIntegral(truncate(days))) * 24
          min = (hours - fromIntegral(truncate(hours))) * 60
          seg = (min - fromIntegral(truncate(min))) * 60

fintyear :: (Real a, Integral b) => a -> (b,b,b,b,b)
fintyear a = (truncate years, truncate days, truncate hours, truncate min, truncate seg)
    where years = fyearsIn a
          days = (years - fromIntegral(truncate(years))) * 365
          hours = (days - fromIntegral(truncate(days))) * 24
          min = (hours - fromIntegral(truncate(hours))) * 60
          seg = (min - fromIntegral(truncate(min))) * 60
--last [1..10^5] Poco
--last [1..10^7] Regular
--last [1..10^20] Mucho
--head [1..10^20] Poco
--last [10^20..1] Poco
--head (tail [1..10^20]) Poco
--length [1..10^20] Mucho
--last (take (10^7) [1..10^20]) Regular
--head (take (10^7) ([1..100] ++ [1..10^20])) Poco
--last (take 100 ([1..10^20] ++ [1..100])) Poco
--last (drop 100 ([1..10^20] ++ [1..100])) Mucho
--head (drop (10^7) ([1..10^20] ++ [1..100])) Regular
--[1..10^7]==[1..10^7] Regular
--[1..10^20]==[1..10^20] Mucho
--[1..10^20]==[1..10^20+1] Mucho
--[1..10^20]==[2..10^20] Poco
--head (reverse [1..10^7]) Regular
--last (reverse [1..10^7]) Regular
--reverse [1..10^20] == reverse [1..10^20+1] Mucho

mediaAr :: (Fractional a) => [a] -> a
mediaAr [] = 0
mediaAr (x:xs) = sum (x:xs) / fromIntegral(length (x:xs))

digitos :: (Integral a) => a -> a
digitos 0 = 0
digitos x = 1 + digitos(div x 10)

reduccion :: (Integral a) => a -> a
reduccion x
      | x < 0 = reduccion (abs(x))
      | x < 10 = x
      | otherwise = reduccion((mod x 10) + (div x 10))

perm :: (Integral a) => a -> a
perm 0 = 1
perm x 
    |x < 0 = error "Solo permutaciones de numeros positivos"
    |otherwise = x * perm (x - 1)

--var :: (Integral a) => a -> a
--var n m = div (perm n) (perm (n - m))