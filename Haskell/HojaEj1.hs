-- Ejercicio1: Escribe una función que dado el radio de un círculo devuelva su area --
ej1 :: Floating a => a -> a
ej1 r = pi*(r**2)

-- Ejercicio 2: Escribe una función que dado un entero devuelva un booleano indicando si el entero --
-- pasado como parámetro es cuadrado perfecto o no. --

ej2 :: Integer -> Bool
ej2 n = fromInteger (round x) == x
    where x = sqrt (fromInteger n)

-- Ejercicio 3: Escribe una función que calcule el número de raíces reales distintas de una ecuación de --
-- segundo grado. Los datos de entrada a la función serán los coeficientes de la ecuación. --

ej3 :: Real a => a -> a -> a -> Int 
ej3 a b c =
    d < 0    = 0 
    d == 0    = 1 
    otherwise    = 2 
where d = b**2 - 4*a*c

--


