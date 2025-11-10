
--Examen Parcial de Programación Declarativa 
--Ejercicio 1 
{- 
Decimos que una lista de números es positiva si y solo si para 
toda terna de elementos consecutivos de la lista se cumple que 
al menos dos de ellos son estrictamente mayores que cero. 
Por ejemplo, la lista [4,-8,8,5,8,-5] es positiva, mientras 
que la lista [8,3,-8,8,0,5,8] no lo es. Escribe una función 
que dada una lista de números de entrada devuelva un booleano 
que indique si la lista de entrada es positiva o no.
-}

ej1 :: [Int] -> Bool 
ej1 []  = False
ej1 [_] = False
ej1 (x:y:xs) 
    | signum x + signum y == 2  = True 
    | otherwise = False 


--Ejercicio 2 
{- 
Escribe una función sufijos que dada una lista de elementos 
devuelva la lista de todos los posibles sufijos de la lista de 
entrada. Por ejemplo sufijos [7,9,2,1] debería devolver 
[[7,9,2,1], [9,2,1], [2,1], [1], []].
-}

ej2 :: [Int] -> [[Int]]
ej2 [] = [[]]
ej2 [x] = [[x],[]]
ej2 xs = xs : ej2 (tail xs)


-- Ejercicio 4 
{-
Un árbol general es una estructura de datos arbórea de􏰂nida de la siguiente forma: 
    data ArbGen a = VacioG | NodoG a [ArbGen a]
Escribe una función takeWhileG que se comporte de manera análoga a la función takeWhile, 
pero para árboles generales. Úsala para hacer una función que se quede solo con toda la 
parte superior de un árbol general mientras cumpla que los elementos de los nodos sean 
estrictamente mayores que 0. Por ejemplo, para el árbol
  NodoG 3 [NodoG 0 [NodoG 3 []],
             NodoG 1 [NodoG 3 [], NodoG (-1) [VacioG,VacioG],VacioG],
             VacioG,
             NodoG 3 [VacioG]]
debería devolver
    NodoG 3 [VacioG,
             NodoG 1 [NodoG 3 [],VacioG,VacioG],
             VacioG,
             NodoG 3 [VacioG]]
-}

data ArbGen a = VacioG | NodoG a [ArbGen a]
  deriving (Eq, Ord, Show, Read)