
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

espos :: Int -> Bool 
espos x
    |x <= 0    = False 
    |x > 0     = True 

ej1 :: [Int] -> Bool 
ej1 []  = False
ej1 [x] = False
ej1 (x:y:xs) 
    | espos x && espos y == True  = True 
    | espos x && espos y == False = ej1 (y:xs)

