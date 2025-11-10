import Data.Array
import Data.List
--Data Conjuntos PODRIAMOS AÑADIR DIRECTAMENTE EL DATA.SET QUE ES EL DATA DE LOS CONJUNTOS
data Cjto a = Cj [a]
    deriving (Eq,Ord,Show,Read)


anadeACjto :: Eq a => a -> Cjto a -> Cjto a
anadeACjto x (Cj xs)
  | elem x xs = Cj xs
  | otherwise = Cj (x:xs)

esCjVacio :: Eq a => Cjto a -> Bool
esCjVacio (Cj []) = True
esCjVacio cjto    = False

eliminaDeCjto :: Eq a => a -> Cjto a -> Cjto a
eliminaDeCjto x (Cj xs) = Cj (elim x xs)
  where elim x []     = []
        elim x (y:ys) 
          | x==y      = ys
          | otherwise = y:elim x ys 

estaEnCjto :: Eq a => a -> Cjto a -> Bool
estaEnCjto x (Cj xs) = elem x xs

elemsDeCjto :: Eq a => Cjto a -> [a]
elemsDeCjto (Cj xs) = xs

eliminaRepeticiones :: Eq a => [a] -> [a]
eliminaRepeticiones xs = elemsDeCjto (foldr anadeACjto creaCjtoVacio xs)

unionCjto :: (Eq a,Ord a) => Cjto a -> Cjto a -> Cjto a
unionCjto (Cj xs) (Cj ys) = Cj (union xs ys)

interseccionCjto :: (Eq a) => Cjto a -> Cjto a -> Cjto a
interseccionCjto (Cj xs) (Cj ys) = Cj (intersect (xs) (ys))

diferenciaCjto :: Eq a => Cjto a -> Cjto a -> Cjto a
diferenciaCjto (Cj xs) (Cj ys) = Cj ((xs)\\(ys))
    
creaCjtoVacio :: Eq a => Cjto a
creaCjtoVacio = Cj []

cardinal :: Cjto a -> Int
cardinal (Cj (xs)) = length xs

mapCjto :: Ord b => (a -> b) -> Cjto a -> Cjto b
mapCjto f (Cj (xs)) = Cj (map f xs)

{-
ERROR NO PODRIAMOS PONER LO DE INT EN LAS CONSTRUCTORAS PUES SI LO HACEMOS EL CASO AFN A AFD SE COMPLICA MUCHISIMO
-}
--Data Movimientos(Distinguiremos si es un AFNE o un AFD/AFN
data Mov a = Movi a Char a -- | MoviC (Cjto Int) Char (Cjto Int)
    deriving (Eq, Ord, Show, Read)
data MovN a = MoviN a Char (Cjto a)
    deriving (Eq, Ord, Show, Read)
data MovE a = MoviE a Char (Cjto a) | EmoviE a (Cjto a)
    deriving (Eq, Ord , Show, Read)


-- DATA AFD
data AFD a = AfdC (Cjto a) (Cjto Char) (Cjto (Mov a)) a (Cjto a)
    deriving (Eq, Ord, Show, Read)

estadosAFD :: AFD a -> Cjto a
estadosAFD (AfdC (Cj (xs)) (Cj ys) (Cj (zs)) x (Cj (ws))) = (Cj (xs))

alfabetoAFD :: AFD a -> Cjto Char
alfabetoAFD (AfdC (Cj (xs)) (Cj ys) (Cj (zs)) x (Cj (ws))) = (Cj (ys))

movimientosAFD :: AFD a -> Cjto (Mov a)
movimientosAFD (AfdC (Cj (xs)) (Cj ys) (Cj (zs)) x (Cj (ws))) = Cj (zs)

estadoInicialAFD :: AFD a -> a
estadoInicialAFD (AfdC (Cj (xs)) (Cj ys) (Cj (zs)) x (Cj (ws))) = x

aceptacionAFD :: AFD a -> Cjto a
aceptacionAFD (AfdC (Cj (xs)) (Cj ys) (Cj (zs)) x (Cj (ws))) = Cj (ws)



--equivalenciaAfdcAfdA :: AFD a -> AFD a
--equivalenciaAfdcAfdA (AfdC (Cj (xs)) (Cj (ys)) (Cj (zs)) x (Cj (ws))) = (AfdA (array (1,5) [(i,i)|i<-[Cj (xs), Cj (ys), Cj (zs), x, Cj (ws)]]))

--DATA AFN
data AFN a = AfnC (Cjto a) (Cjto Char) (Cjto (MovN a)) a (Cjto a) -- | AfnA (Array Int Int)
    deriving (Eq, Ord, Show, Read)

estadosAFN :: AFN a -> Cjto a
estadosAFN (AfnC (Cj (xs)) (Cj ys) (Cj (zs)) x (Cj (ws))) = (Cj (xs))

alfabetoAFN :: AFN a -> Cjto Char
alfabetoAFN (AfnC (Cj (xs)) (Cj ys) (Cj (zs)) x (Cj (ws))) = (Cj (ys))

movimientosAFN :: AFN a -> Cjto (MovN a)
movimientosAFN (AfnC (Cj (xs)) (Cj ys) (Cj (zs)) x (Cj (ws))) = Cj (zs)

estadoInicialAFN :: AFN a -> a
estadoInicialAFN (AfnC (Cj (xs)) (Cj ys) (Cj (zs)) x (Cj (ws))) = x

aceptacionAFN :: AFN a -> Cjto a
aceptacionAFN (AfnC (Cj (xs)) (Cj ys) (Cj (zs)) x (Cj (ws))) = Cj (ws)
--equivalenciaAfncAfnA :: AFN a -> AFN a
--equivalenciaAfncAfnA (AfdC (Cj (xs)) (Cj (ys)) (Cj (zs)) x (Cj (ws))) = (AfdA (array (1,5) [Cj (xs), Cj (ys), Cj (zs), x, Cj (ws)]))

--DATA AFN-E
data AFNE a = AfneC (Cjto a) (Cjto Char) (Cjto (MovE a)) a (Cjto a) -- | AfneA (Array Int Int)
    deriving (Eq, Ord, Show, Read)
    
estadosAFNE :: AFNE a -> Cjto a
estadosAFNE (AfneC (Cj (xs)) (Cj ys) (Cj (zs)) x (Cj (ws))) = (Cj (xs))

alfabetoAFNE :: AFNE a -> Cjto Char
alfabetoAFNE (AfneC (Cj (xs)) (Cj ys) (Cj (zs)) x (Cj (ws))) = (Cj (ys))

movimientosAFNE :: AFNE a -> Cjto (MovE a)
movimientosAFNE (AfneC (Cj (xs)) (Cj ys) (Cj (zs)) x (Cj (ws))) = Cj (zs)

estadoInicialAFNE :: AFNE a -> a
estadoInicialAFNE (AfneC (Cj (xs)) (Cj ys) (Cj (zs)) x (Cj (ws))) = x

aceptacionAFNE :: AFNE a -> Cjto a
aceptacionAFNE (AfneC (Cj (xs)) (Cj ys) (Cj (zs)) x (Cj (ws))) = Cj (ws)
    
--equivalenciaAfnecAfneA :: AFD a -> AFD a
--equivalenciaAfnecAfneA (AfdC (Cj (xs)) (Cj (ys)) (Cj (zs)) x (Cj (ws))) = (AfdA (array (1,5) [Cj (xs), Cj (ys), Cj (zs), x, Cj (ws)]))

--DATA ER
data ER = Vacio | Epsilon | Term Char | Eleccion ER ER | Concat ER ER | Cierre ER | Parentesis ER
    deriving (Eq, Ord, Show, Read)
{-
--ER A AFNE /// ME SALE EL SIGUIENTE ERROR: Inferred type not general enough
erAFNE :: (Num a, Ord a) => ER -> AFNE a
erAFNE (Vacio) = AfneC (Cj [0]) (Cj []) (Cj []) 0 (Cj [])
erAFNE (Epsilon) = AfneC (Cj[0]) (Cj []) (Cj []) 0 (Cj [0])
erAFNE (Term x) = AfneC (Cj [0,1]) (Cj [x]) (Cj [MoviE 0 x (Cj [1])]) 0 (Cj [1])
erAFNE (Concat x y) = concatenacion (erAFNE x) (erAFNE y)
erAFNE (Eleccion x y) = eleccion (erAFNE x) (erAFNE y)
erAFNE (Cierre x) = cierre (erAFNE x)

concatenacion :: (Num a, Ord a) => AFNE a -> AFNE a -> AFNE a
concatenacion (AfneC (estados1) (alfabeto1) (mov1) x (aceptacion1)) (AfneC (estados2) (alfabeto2) (mov2) y (aceptacion2)) = (AfneC (unionCjto (estados1) (estados22)) (unionCjto (alfabeto1) (alfabeto2)) (unionCjto (mov1) (mov22)) x (aceptacion22))
    where m1 = cardinal (estados1)
          estados22 = mapCjto (suma m1+1) estados2
          mov22 = mapCjto (sumamov m1+1) mov2
          aceptacion22 = mapCjto (\x->x+1) aceptacion2
suma :: (Num a, Ord a) => a -> a -> a
suma n m = n+m
sumamov :: (Num a, Ord a) => a -> MovE a -> MovE a
sumamov n (EmoviE m p) = EmoviE (m+n) (mapCjto (\x->x+n) (p))
sumamov n (MoviE m c p) = MoviE (m+n) c (mapCjto (\x->x+n) (p))


eleccion :: (Num a, Ord a) => AFNE a -> AFNE a -> AFNE a
eleccion (AfneC (estados1) (alfabeto1) (mov1) x (aceptacion1)) (AfneC (estados2) (alfabeto2) (mov2) y (aceptacion2)) = AfneC (unionCjto (unionCjto (estados11) (estados22)) (Cj [0])) (unionCjto (alfabeto1) (alfabeto2)) (unionCjto (unionCjto (mov11) (mov22)) (nuevomov)) 0 (unionCjto (aceptacion11) (aceptacion22))
    where m1= cardinal(estados1)
          estados11 = mapCjto (suma 1) estados1
          estados22 = mapCjto (suma m1+1) estados2
          aceptacion11 = mapCjto (suma 1) aceptacion1
          aceptacion22 = mapCjto (suma m1+1) aceptacion2
          nuevomov = (Cj [EmoviE 0 (Cj [1]),EmoviE 0 (Cj [m1+1])])
          mov11 = mapCjto (sumamov 1) (mov1)
          mov22 = mapCjto (sumamov m1+1) (mov2)
          
sumamov :: (Num a, Ord a) => a -> MovE a -> MovE a
sumamov n (EmoviE m p) = EmoviE (m+n) (mapCjto (\x->x+n) (p))
sumamov n (MoviE m c p) = MoviE (m+n) c (mapCjto (\x->x+n) (p))

suma :: (Num a, Ord a) => a -> a -> a
suma n m = n+m

cierre :: (Num a, Ord a) => AFNE a -> AFNE a
cierre (AfneC (estados) (alfabeto) (mov) x (aceptacion)) = (AfneC (unionCjto (Cj[0]) (estados1)) (alfabeto) (unionCjto (unionCjto (mov1) (nuevomov)) (Cj [EmoviE 0 (Cj [1])])) 0 (Cj [0]))
    where estados1 = mapCjto (suma 1) estados
          mov1 = mapCjto (sumamov 1) mov
          aceptacion1 = mapCjto (suma 1) aceptacion
          nuevomov = mapCjto (nuevosmov) (mapCjto (suma 1) aceptacion)

nuevosmov :: Num a => a -> MovE a
nuevosmov n = EmoviE n (Cj [0])
-}
{--AFD A ER
afdER :: (Num a, Ord a) => AFD a -> ER
afdER (AfdC (Cj [0]) (Cj []) (Cj []) 0 (Cj [])) = Vacio
afdER (AfdC (Cj [0]) (Cj []) (Cj []) 0 (Cj [0])) = Epsilon
afdER (AfdC (Cj [0,1]) (Cj ['a']) (Cj [Movi 0 'a' 1]) 0 (Cj [1])) = Term 'a'
-}
{- COSO JURJO
minimizarEstados ::  Cjto (Mov Int) -> Int -> Cjto Int
minimizarEstados (Cj []) _ = (Cj [])
minimizarEstados (Cj ((Movi t _ m):xs)) n
    |t == n = unionCjto (Cj [m]) (minimizarEstados (Cj (xs)) n)
    |otherwise = (minimizarEstados (Cj (xs)) n)
-}