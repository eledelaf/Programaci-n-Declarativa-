{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
mas1 :: Int -> Int  --Devuelve x+1
mas1 x = x+1

f :: Int -> Int -> Int     --Devuelve (x*y)+3
f x y = x * y + 3

esBisiesto :: Int -> Bool               --Devuelve un booleano en funcion de si es bisiesto o no 
esBisiesto x = (mod x 4 == 0) && (mod x 100 /= 0) || (mod x 400 == 0)

identidad :: a -> a   -- Devulve la identidad 
identidad x = x

aMayuscula :: Char -> Char                   --Dado una letra en minuscula devuelve la letra en mayuscula
aMayuscula x = toEnum(diferencia + fromEnum x)
    where diferencia = fromEnum 'A' - fromEnum 'a'  -- La diferencia entre A y a es la misma que entre B y b...

otraForma :: Char -> Char                -- Dado una letra en minuscula devuelve la mayuscula
otraForma x = let diferencia = fromEnum 'A' - fromEnum 'a' 
    in toEnum (diferencia + fromEnum x)

{- if-then-else 
if (esto pasa) then (devolver esto) else (si no pasa lo del if devolver otra cosa)
-}   
mayor :: Int -> Int -> Int
mayor x y = if x>y then x else y

{- Guardas: 
    Se evaluan de arriba a bajo, primero se mira la de arriba si se cumple devuelve lo que hay 
    así sucesivamente. No lee nada que no necesita
-}
mayor' :: Int -> Int -> Int 
mayor' x y
    | x > y    = x
    |otherwise = y              -- otherwise, se usa si todo lo de arriba no ha funcionado esto si

signo :: Int -> Int
signo x 
    |x > 0  = 1
    |x < 0  = -1
    |x == 0 = 0  

{- Case
Si z == 0 devuelve x*y, si z == 1 devuelve x+y, si no x-y 
-}
g :: Int -> Int -> Int -> Int
g x y z = case z of 
    0 -> x*y
    1 -> x+y 
    otherwise -> x-y

{-Patrones
Otra forma de hacer el ejercicio anterior es leer la primera igualdad si se
cumple devolvemos ese resultado, si no pasamos a la siguiente y así sucesivamente
-}
g' :: Int -> Int -> Int -> Int
g' x y 0 = x*y
g' x y 1 = x+y
g' x y z = x-y


factorial :: Int -> Int  --Devuelve el factorial de un número 
factorial 0 = 1                 -- Caso base
factorial n = n * factorial (n-1)    -- Caso iterativo 


fib :: Int -> Int
fib 0 = 0                           -- Casos bases
fib 1 = 1                           -- Casos bases
fib n = fib (n-1) + fib (n-2)       -- Caso iterativo 

repetir :: Int -> a -> [a]
repetir 0 x = []
repetir n x = x : repetir (n-1) x

{- Esta es la función predefinida como sum -}
sumatorio :: [Int] -> Int 
sumatorio [a]   = a
sumatorio (x:xs) = x + sumatorio xs 

{- Esta es la función predefinida como lenght -}
longitud :: [Int] -> Int
longitud [] = 0 
longitud (x:xs) = 1 + longitud xs

alReves :: [a] -> [a]
alReves [] = []
alReves (x:xs) = alReves xs ++ [x]

{- Esta es la función predefinida como take n [Int], 
es la función que coje los n primeros terminos de [Int]
-}
coger :: Int -> [a] -> [a]
coger 0 _  = []
coger _ [] = []
coger n (x:xs) = x : coger (n-1) xs 

{- Esta es la función predefinida como drop n [Int], 
es la función que elimina los n primero terminos de [Int]
-}
saltar :: Int -> [a] -> [a]
saltar 0 xs = xs
saltar _ [] = []
saltar n (x:xs) = saltar (n-1) xs

{-Dado un n y una lista te devuelve una tupla con una lista de los n primeros elementos
y otra lista con el resto
-}
partir :: Int -> [a] -> ([a],[a])
partir 0 xs = ([],xs)
partir _ [] = ([],[])
partir n (x:xs) = (x:iz, dr)
    where (iz,dr) = partir (n-1) xs

{- Dadas dos listas devuelve una lista de tuplas con los numeros de las listas 
-}
cremallera :: [a] -> [a] -> [(a,a)]
cremallera (x:xs) (y:ys) = (x,y) : cremallera xs ys
cremallera _ _ = []

anticremallera :: [(a,a)] -> ([a],[a])
anticremallera [] = ([],[])
anticremallera ((a,b):xs) = (a:as,b:bs)
    where (as,bs) = anticremallera xs

insertSort :: [Int] -> [Int]
insertSort [] = []
insertSort (x:xs) = inserta x (insertSort xs)

inserta :: Int -> [Int] -> [Int]
inserta x [] = [x]
inserta x (y:ys)
    |x <= y = x : y : ys 
    | otherwise = y : inserta x ys 
