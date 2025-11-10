{-# LANGUAGE RankNTypes #-}
import Distribution.Simple.Utils (xargs)
import System.Directory.Internal.Prelude (Num)
--Apuntes
factorial :: (Eq p, Num p) => p -> p
factorial 0 = 1
factorial n = n * factorial (n-1)

sumatorio :: Num p => [p] -> p
sumatorio = foldr (+) 0

fact 0 = 1
fact n = n * fact (n-1)

unoMas :: [Int] -> [Int]
unoMas xs = map (+ 1) xs

unoMas' :: [Int] -> [Int]
unoMas' = map f
    where f x = x+1



cuadrado :: [Int] -> [Int]
cuadrado xs = map (\ x -> x * x) xs


cuadrado' :: Num b => [b] -> [b]
cuadrado' = map g
    where g x = x*x
    
-- Arboles binarios 
--data ArbBinEnt = AVacioE | Nodo Int ArbBinEnt 

-- No funciona 
--profundidad :: ArbBinEnt -> Int
--profundidad AVacioE = 0
--profundidad (NodoE iz dr) = 1 + max iz (profundidad dr)


-- Arboles binarios de busqueda
data ABB a = Vacio | Nodo a (ABB a) (ABB a)
  deriving (Eq,Show,Read)

estaVacio :: ABB a -> Bool
estaVacio Vacio = True
estaVacio _ = False

crearABBVacio :: ABB a
crearABBVacio = Vacio

anadir :: Ord a => a -> ABB a -> ABB a
anadir x Vacio = Nodo x Vacio Vacio
anadir x (Nodo y iz dr)
  | x > y = Nodo y iz (anadir x dr)
  | otherwise = Nodo y (anadir x iz) dr
  
eliminar :: Ord a => a -> ABB a -> ABB a
eliminar x Vacio = Vacio
eliminar x (Nodo y iz dr)
  | x > y = Nodo y iz (eliminar x dr)
  | x < y = Nodo y (eliminar x iz) dr
  | estaVacio iz = dr
  | otherwise = Nodo maxI (eliminar maxI iz) dr
  where maxI = maximo iz

maximo :: ABB a -> a
maximo (Nodo x iz dr)
  | estaVacio dr = x
  | otherwise = maximo dr  
  
inOrden :: ABB a -> [a]
inOrden Vacio = []
inOrden (Nodo x iz dr) = inOrden iz ++ [x] ++ inOrden dr
