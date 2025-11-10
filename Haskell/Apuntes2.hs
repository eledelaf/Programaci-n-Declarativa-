import Distribution.SPDX.LicenseId (LicenseId(XSkat))
import Distribution.SPDX (LicenseId(XSkat))
--Apuntes2


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
  
inOrden :: Ord a => ABB a -> [a]
inOrden Vacio = []
inOrden (Nodo x iz dr) = inOrden iz ++ [x] ++ inOrden dr

buscar :: Ord a => a -> ABB a -> Bool 
buscar _ Vacio = False 
buscar x (Nodo y iz dr)
    |x == y = True 
    |otherwise  = buscar x iz || buscar x dr 

meterListaABB :: Ord a => [a] -> ABB a
meterListaABB [] = Vacio 
meterListaABB (x:xs) = anadir x (meterListaABB xs)


ochoreinas :: [[(Int,Int)]]
ochoreinas = reinas 8 

reinas :: Int -> [[(Int,Int)]]
reinas 0 = [[]]
reinas n = [(n,nuevo): previo| previo <- reinas(n-1), nuevo <-[1..8], noAtaques previo (n,nuevo)]

noAtaques :: [(Int,Int)] -> (Int,Int) -> Bool 
noAtaques lista nueva = and [comprueba previa nueva| previa <- lista]

comprueba :: (Int, Int) -> (Int, Int) -> Bool
comprueba (i,j) (m,n) = (j /= n) && (i-j /= m-n)

