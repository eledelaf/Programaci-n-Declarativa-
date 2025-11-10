{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Distribution.SPDX (LicenseId(XSkat))
import GHC.Real (Integral)
import Text.Read.Lex (Number)

f x = x+ x

-- Ejercicio 1
ej1 :: Floating a => a -> a
ej1 r = pi*r^2

-- Ejercicio 2
ej2 :: Integer -> Bool
ej2 n = fromInteger (round x) == x
  where x = sqrt (fromInteger n)

-- Ejercicio 3
ej3 :: Real a => a -> a -> a -> Int 
ej3 a b c
  |d == 0    = 0
  |d < 0  = 1
  |d > 0  = 2
  where d = b^2 - 4*a*c


-- Ejercicio 4
--(length('a':[])):[]

-- Ejercicio 5
-- Ejercicio 6
-- Ejercicio 7
ej7 :: [a] -> [b] -> [c] -> [(a,b,c)]
ej7 (x:xs) (y:ys) (z:zs) = (x,y,z) : ej7 xs ys zs
ej7 _ _ _ = []

-- Ejecicio 8
ej8 :: Integral a => [a] -> a
ej8 [] = 1
ej8 (x:xs) = ej8 xs * x

-- Ejercicio 9
ej9 :: Num a => [a] -> [a] -> a
ej9 [] ys = 0
ej9 xs [] = 0
ej9 (x:xs) (y:ys) = x*y + ej9 xs ys

-- Ejercicio 10
ej10 :: [a] -> [a] -> [a]
ej10 [] ys = ys
ej10 (x:xs) ys = x : ej10 xs ys

-- Ejercicio 11
ej11 :: [[a]] -> [a]
ej11 [] = []
ej11 (x:xs) = x ++ ej11 xs

-- Ejercicio 12
ej12 :: Ord a => [a] -> Bool
ej12 [] = True
ej12 [x] = True
ej12 (x:xs:xss) = (x<xs) && ej12 (xs:xss)

-- Ejercicio 13 
ej13 :: Integral a => [a] -> [a]
ej13 [] = []
ej13 (x:xs)
    | even x    = ej13 xs
    | otherwise = x : ej13 xs

ej13' :: Integral a => [a] -> [a]
ej13' [] = []
ej13' (x:xs)
    | even x    = x : ej13' xs
    | otherwise = ej13' xs


 --Ejercicio 14
ej14 :: Integral a => [a] -> [a]
ej14 xs = alterna pares impares
  where pares   = ej13' xs
        impares = ej13  xs
-- Precondicion: Las dos listas de entrada son de la misma longitud
alterna :: [a] -> [a] -> [a]
alterna [] [] = []
alterna (x:xs) (y:ys) = x : y : alterna xs ys

--Ejercicio 15
ej15 :: Integral a => a -> a
ej15 x = 1 + ej15' (abs x)

ej15' :: Integral a => a -> a
ej15' x
  | x < 2 = 1
  | otherwise = 1 + ej15' (x `div` 2)

 --Ejercicio 16
ej16 :: Integral a => a -> [a]
ej16 0 = [0]
ej16 1 = [1]
ej16 x
    |even x = ej16 (x `div` 2)  ++ [0]
    |odd x  = ej16 (x `div` 2) ++ [1]

--Ejercicio 17
delete' :: (Eq a) => a -> [a] -> [a]
delete' y [] = []
delete' y (x:xs) = if x == y then xs else x : delete' y xs

ej17 :: Integral a => [a] -> [a]
ej17 [] = []
ej17 x = m : ej17 (delete' m x)
    where m = minimum x

--Ejercicio 18 
takeLast :: Int -> [a] -> [a]
takeLast 0 _  = []
takeLast n [] = []
takeLast n xs = take n (reverse xs)

--Ejercicio 19 
dropLast :: Int -> [a] -> [a]
dropLast 0 xs = xs
dropLast _ [] = []
dropLast n xs = dropLast' m xs
    where m = length (xs) - n

dropLast':: Int -> [a] -> [a]
dropLast' _ [] = []
dropLast' 0 _ = []
dropLast' n (x:xs) = x : dropLast' (n-1) xs

--Ejercicio 20 
genFib :: Int -> Int -> [Int]
genFib m n = m : genFib n (n+m)

--Ejercicio 21 
ej21 :: Integral a => a -> a -> a -> a
ej21 a b c = gcd a (gcd b c)

ej21' :: Integral a => a -> a -> a -> a
ej21' a b c = ej21'' a (ej21'' b c)

ej21'' :: Integral a => a -> a -> a
ej21'' a b = div (a*b) (gcd a b)

-- Ejercicio 22 
simplificar (a,b) = (div a z, div b z)
    where z = gcd a b

suma :: Integral a => (a, a) -> (a, a) -> (a, a)
suma (a,b) (c,d) = simplificar (w,z)
    where z = ej21'' b d
          w = div (a*z) b + div (c*z) d

resta :: Integral a => (a,a) -> (a,a) -> (a,a)
resta (a,b) (c,d) = simplificar (w,z)
    where z = ej21'' b d
          w =  div (a*z) b - div (c*z) d

multiplicacion :: Integral a => (a,a) -> (a,a) -> (a,a)
multiplicacion (a,b) (c,d) = simplificar (a*c, b*d)

division :: Integral a => (a,a) ->(a,a) -> (a,a)
division (a,b) (c,d) = multiplicacion (a,b) (d,c)

-- Ejercicio 23
ej23 :: (Integer,Integer) -> Integer -> (Integer,Integer)
ej23 (a,b) n 
  |n == 0  = (1,1)
  |n < 0   = simplificar (b^(-n), a^(-n))
  |n > 0   = simplificar (a^n , b^n)

--Ejercicio 24 
ej24 :: [Int] -> Int 
ej24 [] = 0
ej24 (x:xs) = max x (ej24 xs)

ej24' :: [Int] -> Int 
ej24' [] = 0
ej24' (x:xs) = min x (ej24 xs)

--Ejercicio 25
ej25 :: [Int] -> Int 
ej25 [] = 1
ej25 (x:xs) = gcd x (ej25 xs)

ej25' :: [Int] -> Int 
ej25' [] = 1
ej25' (x:xs) = ej21'' x (ej25' xs)
