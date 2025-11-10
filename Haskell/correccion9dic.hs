{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- Correción examen PRDE 
-- Examen 9 diciembre 2020
{-Dadas dos listas, decimos que están doblemente ordenadas si y solo si 
cada lista está ordenada de menor a mayor y no existe ninguna posición i tal 
que el valor de la posición i−ésima de la primera lista sea estrictamente mayor 
que el valor de la posición i-ésima de la segunda lista. 
Por ejemplo, las listas [1,5,9,14] y [2,7,10] están doblemente ordenadas, 
mientras que las listas [1,5,9,14] y [2,3,10] no lo están. 
Escribe una función que dadas dos listas de elementos de entrada devuelva un booleano que indique si están 
doblemente ordenadas o no.-}

dobleOrd :: Ord a => [a] -> [a] -> Bool -- Necesitamos que el tipo tenga orden por eso ponemos Ord a
dobleOrd xs ys = ordenada xs && ordenada ys && orden2 xs ys -- Hay que definir las funciones auxiliares 

ordenada :: Ord a => [a] -> Bool
ordenada []  = True
ordenada [x] = True
ordenada (x:y:xs) = x <= y && ordenada(y:xs) -- Deja el termino y pq hay que compraralo conel siguiente 

-- Hecho por mi 

orden2 :: Ord a => [a] -> [a] -> Bool
orden2 [] _ = True
orden2 _ [] = True
orden2 (x:xs) (y:ys) = x <= y && orden2 xs ys

-- Hecho por Natalia se puede hacer con la función predefinida zipWith 

{-
Escribe una función numeroBinario que dada una lista de ceros y unos devuelva 
el número entero que representa dicha lista en binario. Por ejemplo numeroBinario 
[1,1,1,0,1] debería devolver 29.
-}

numeroBinario :: [Int] -> Int      --((0*2+1)*2+1)*2+1... esto es un foldl
--numeroBinario xs = foldl (\x y=2*x+y) 0 xs -- La función que queremos implementar, donde empieza y la lista 
numeroBinario xs = foldl f 0 xs    --Esto me gusta más pq lo veo más claro
    where f x y = 2*x +y

{-Define un nuevo tipo de datos para representar conjuntos. 
Implementa operaciones para crear un conjunto vacío, para añadir un elemento a un conjunto, 
para eliminar un elemento del conjunto, y para obtener la lista de elementos de 
un conjunto. Utiliza dichas funciones para implementar una función que dada una 
lista de elementos devuelva otra lista equivalente pero en la que ningún elemento 
aparezca más de una vez.-}
-- Hecho en clase
{-
Un árbol general es una estructura de datos arbórea de􏰂nida de la siguiente forma: data ArbGen a = VacioG | NodoG a [ArbGen a]
Escribe una función aplanarG que dado un árbol general devuelva la lista de todos los elementos que aparecen en alguno de sus nodos. Por ejemplo, para el árbol
    NodoG 3 [NodoG 0 [NodoG 3 []],
             NodoG 1 [NodoG 3 [], NodoG (-1) [VacioG,VacioG],VacioG],
             VacioG,
             NodoG 3 [VacioG]]
debería devolver [3,0,3,1,3,-1,3]
-}

data ArbGen a = VacioG | NodoG a [ArbGen a]
aplanarG :: ArbGen a -> [a]
aplanarG VacioG = []
aplanarG (NodoG x hijos) = x: concat ( map aplanarG hijos )

{-
Dada una matriz bidimensional, decimos que es algoCreciente si para alguna de sus las se cumple que 
el sumatorio de todos sus elementos es estrictamente menor que el sumatorio de todos los elementos 
de la fila siguiente.-}
{-
algoCreciente ::  ( Num a, Ord a ) => [a] -> Bool
-- Cuando tengo un elemento = x, cuando tengo una lista = xs, cuando tengo una lista de listas = xss, etc

algoCreciente (xs:ys:xss) =  if suma xs < suma ys then True else False  or algoCreciente (ys:xss) --Idea mia 
    where suma xs = foldr (+) 0
algoCreciente xss = or (zipWith (<) sumas (tail sumas))
    where sumas = map (foldr (+) 0) xss

-- No funciona
-}


{-En un fichero de entrada tenemos almacenada una lista de números reales, de modo que los números 
están separados por un espacio en blanco. Un posible 􏰂chero de entrada sería el siguiente:
  34.3 1.5 173 12.2
Escribe una función que pregunte al usuario el nombre de un fichero de texto de entrada, lea dicho 
fichero y muestre por pantalla la media de dichos números reales. Por ejemplo, para el fichero anterior 
debería mostrar lo siguiente:
55.25
-}

--Primero lo paso a trozos y luego lo transforma a Float
procesa :: IO()
procesa = do putStr "Dame el nombre del fichero"
         nombre    <- getLine 
         contenido <- readFile nombre 
         let numeros :: [Float]
             numeros = map read ( words contenido )
         print sum numeros / (fromIntegral (length numeros))