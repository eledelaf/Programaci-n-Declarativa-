import Data.List
import Data.Maybe
--1.Creación del tipo de datos de conjuntos, así como las funciones necesarias en el trabajo sobre mismo tipo de datos.
data Cjto a = Cj [a]
    deriving (Eq,Ord,Show,Read)

anadeACjto :: Eq a => a -> Cjto a -> Cjto a
anadeACjto x (Cj xs)
  | elem x xs = Cj xs
  | otherwise = Cj (x:xs)

esCjVacio :: Eq a => Cjto a -> Bool
esCjVacio (Cj []) = True
esCjVacio cjto    = False

noEsCjVacio :: Eq a => Cjto a -> Bool
noEsCjVacio (Cj []) = False
noEsCjVacio cjto    = True

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

eliminaRepeticionesCjto :: Eq a => Cjto a -> Cjto a
eliminaRepeticionesCjto (Cj xs) = Cj (eliminaRepeticiones xs)

unionCjto :: (Eq a,Ord a) => Cjto a -> Cjto a -> Cjto a
unionCjto (Cj xs) (Cj ys) = Cj (union xs ys)

interseccionCjto :: (Eq a) => Cjto a -> Cjto a -> Cjto a
interseccionCjto (Cj xs) (Cj ys) = Cj (intersect (xs) (ys))

diferenciaCjto :: Eq a => Cjto a -> Cjto a -> Cjto a
diferenciaCjto (Cj xs) (Cj ys) = Cj ((xs)\\(ys))

creaCjtoVacio :: Eq a => Cjto a
creaCjtoVacio = Cj []

cardinal :: (Num b, Ord b) => Cjto a -> b
cardinal (Cj (xs)) = (longitud (xs))

mapCjto :: Ord b => (a -> b) -> Cjto a -> Cjto b
mapCjto f (Cj (xs)) = Cj (map f xs)

filterCjto :: (a -> Bool) -> Cjto a -> Cjto a
filterCjto f (Cj (xs)) = Cj (filter f xs)

foldl1Cjto :: (a -> a -> a) -> Cjto a ->  a
foldl1Cjto f (Cj xs) = (foldl1 f xs)

primerElemento :: Cjto a -> a
primerElemento (Cj (x:xs)) = x

maximoCjto :: Ord a => Cjto a -> a
maximoCjto (Cj xs) = maximum xs

longitud :: (Num b,Ord b) => [a] -> b
longitud [] = 0
longitud (x:xs) = 1 + longitud(xs)

unionmulti :: (Eq a, Ord a) => (Cjto (Cjto a)) -> (Cjto a)
unionmulti (Cj []) = (Cj [])
unionmulti (Cj (x:xs)) = (unionCjto (x) (unionmulti (Cj (xs))))

--2.Creación del tipo de datos de autómatas, así como las funciones necesarias en el trabajo sobre el mismo tipo de datos.
--2.1.AFN-E
--2.1.1.Movimientos AFN-E
data MovE a = MoviE a Char (Cjto a) | EmoviE a (Cjto a)
    deriving (Eq, Ord , Show, Read)

estadolibre :: (MovE a) -> a
estadolibre (EmoviE (q) _) = q

conjuntolibre :: (MovE a) -> (Cjto a)
conjuntolibre (EmoviE _ (Cj (ms))) = (Cj (ms))

estadopartidanolibre :: (MovE a) -> a
estadopartidanolibre (MoviE q _ _) = (q)

caracterdeMovE :: (MovE a) -> Char
caracterdeMovE (MoviE _ r _) = r

conjuntodemovsnolibres :: (MovE a) -> (Cjto a)
conjuntodemovsnolibres (MoviE _ _ (Cj (ms))) = (Cj (ms))

estransicionlibre :: (Eq a, Ord a) => (MovE a) -> Bool
estransicionlibre (EmoviE _ _) = True 
estransicionlibre (MoviE _ _ _) = False

noestransicionlibre :: (Eq a, Ord a) => (MovE a) -> Bool
noestransicionlibre (MoviE _ _ _) = True
noestransicionlibre (EmoviE _ _) = False

transicioneslibres :: (Eq a, Ord a) => (Cjto (MovE a)) -> (Cjto (MovE a))
transicioneslibres (Cj (ms)) = (filterCjto (estransicionlibre) (Cj (ms)))

transicionesnolibres :: (Eq a, Ord a) => (Cjto (MovE a)) -> (Cjto (MovE a))
transicionesnolibres (Cj (ms)) = (filterCjto (noestransicionlibre) (Cj (ms)))

estadoSalida :: MovE a -> a
estadoSalida (MoviE x y z) = x
estadoSalida (EmoviE x y) = x

equivalenciaMER :: Mov a -> Mov a
equivalenciaMER (Movi a x b) = (MoviER a (Term x) b)
equivalenciaMER (MoviER a x b) = (MoviER a x b)

ordenarMovE :: Ord a => Cjto a -> Cjto (MovE a) -> Cjto (MovE a)
ordenarMovE  _  (Cj []) = (Cj [])
ordenarMovE (Cj (xs)) (Cj (ms)) = (unionmulti (mapCjto (orden (Cj (ms))) (Cj (xs))))

orden :: Ord a => Cjto (MovE a) -> a -> Cjto (MovE a)
orden (Cj (ms)) x = ordenar (buscarEntrada (Cj (ms)) x)

buscarEntrada :: Eq a => Cjto (MovE a) -> a -> Cjto (MovE a)
buscarEntrada (Cj []) _ = Cj []
buscarEntrada (Cj (m:ms)) x
    |estadoSalida(m) == x = anadeACjto m (buscarEntrada (Cj (ms)) x)
    |otherwise = buscarEntrada (Cj (ms)) x

ordenar :: Ord a => Cjto (MovE a) -> Cjto (MovE a)
ordenar (Cj []) = Cj []
ordenar (Cj (ms)) = anadeACjto (m) (ordenar (eliminaDeCjto (m) (Cj (ms))))
    where m = minimum (ms)

--2.1.2.Tipo de datos: AFN-E (Autómata finito no determinista con transiciones libres)
data AFNE a = AfneC (Cjto a) (Cjto Char) (Cjto (MovE a)) a (Cjto a) 
    deriving (Eq, Ord, Show, Read)
 
suma :: (Num a, Ord a) => a -> AFNE a -> AFNE a
suma n (AfneC (estados1) (alfabeto1) (mov1) x (aceptacion1)) = AfneC (estados) (alfabeto1) (mov) (x+n) (aceptacion)
    where estados = mapCjto (sumaE n) (estados1)
          mov = mapCjto (sumaM n) (mov1)
          aceptacion = mapCjto (sumaE n) (aceptacion1)
          
sumaE :: (Num a, Ord a) => a -> a -> a
sumaE n m = n+m

sumaM :: (Num a, Ord a) => a -> MovE a -> MovE a
sumaM n (MoviE m x (xs)) = MoviE (m+n) x (mapCjto (sumaE n) (xs))
sumaM n(EmoviE m (xs)) = EmoviE (m+n) (mapCjto (sumaE n) (xs))
    
estadosAFNE :: AFNE a -> Cjto a
estadosAFNE (AfneC (Cj (xs)) (Cj ys) (Cj (zs)) x (Cj (ws))) = (Cj (xs))

alfabetoAFNE :: AFNE a -> Cjto Char
alfabetoAFNE (AfneC (Cj (xs)) (Cj ys) (Cj (zs)) x (Cj (ws))) = (Cj (ys))

movimientosAFNE :: AFNE a -> Cjto (MovE a)
movimientosAFNE (AfneC (Cj (xs)) (Cj ys) (Cj (zs)) x (Cj (ws))) = (Cj (zs))

estadoInicialAFNE :: AFNE a -> a
estadoInicialAFNE (AfneC (Cj (xs)) (Cj ys) (Cj (zs)) x (Cj (ws))) = x

aceptacionAFNE :: AFNE a -> Cjto a
aceptacionAFNE (AfneC (Cj (xs)) (Cj ys) (Cj (zs)) x (Cj (ws))) = (Cj (ws))

numEstadosAFNE :: (Num b,Ord b) => AFNE a -> b
numEstadosAFNE (AfneC (e) (a) (m) x (ac)) = (cardinal (e))

nuevosMov :: (Num a, Ord a) => a -> a -> MovE a
nuevosMov n m = (EmoviE m (Cj [n]))

--2.2.AFN 
--2.2.1.Movimientos AFN 
data MovN a = MoviN a Char (Cjto a)
    deriving (Eq, Ord, Show, Read)

dameEstadopartia :: MovN a -> a
dameEstadopartia (MoviN x y z) = x
    
caracterdeMovN :: MovN a -> Char
caracterdeMovN (MoviN x y z) = y

conjuntodeaMovN :: MovN a -> (Cjto a)
conjuntodeaMovN (MoviN x y z) = z

estadoinitransN :: MovN a -> (Cjto a)
estadoinitransN (MoviN x y z) = (Cj [x])

--2.2.2.AFN: AFN (Autómata finito no determinista)
data AFN a = AfnC (Cjto a) (Cjto Char) (Cjto (MovN a)) a (Cjto a) 
    deriving (Eq, Ord, Show, Read)

estadosAFN :: AFN a -> Cjto a
estadosAFN (AfnC (Cj (xs)) (Cj ys) (Cj (zs)) x (Cj (ws))) = (Cj (xs))

alfabetoAFN :: AFN a -> Cjto Char
alfabetoAFN (AfnC (Cj (xs)) (Cj ys) (Cj (zs)) x (Cj (ws))) = (Cj (ys))

movimientosAFN :: AFN a -> Cjto (MovN a)
movimientosAFN (AfnC (Cj (xs)) (Cj ys) (Cj (zs)) x (Cj (ws))) = (Cj (zs))

estadoInicialAFN :: AFN a -> a
estadoInicialAFN (AfnC (Cj (xs)) (Cj ys) (Cj (zs)) x (Cj (ws))) = x

aceptacionAFN :: AFN a -> Cjto a
aceptacionAFN (AfnC (Cj (xs)) (Cj ys) (Cj (zs)) x (Cj (ws))) = (Cj (ws))

sumaFN :: (Num a, Ord a) => a -> AFN a -> AFN a
sumaFN n (AfnC (estados1) (alfabeto1) (mov1) x (aceptacion1)) = AfnC (estados) (alfabeto1) (mov) (x+n) (aceptacion)
    where estados = mapCjto (sumaE n) (estados1)
          mov = mapCjto (sumaMFN n) (mov1)
          aceptacion = mapCjto (sumaE n) (aceptacion1)
          
sumaEFN :: (Num a, Ord a) => a -> a -> a
sumaEFN n m = n+m

sumaMFN :: (Num a, Ord a) => a -> MovN a -> MovN a
sumaMFN n (MoviN m x (xs)) = MoviN (m+n) x (mapCjto (sumaE n) (xs))

--2.3.AFD
--2.3.1.Movimientos AFD
data Mov a = Movi a Char a | MoviER a ER a
    deriving (Eq, Ord, Show, Read)

damellegada :: Mov a -> a
damellegada (Movi x y z) = z

auxx :: (Mov (Cjto a)) -> (Cjto a)
auxx (Movi x y z) = z

obtenerER :: Mov a -> ER
obtenerER (Movi a x b) = obtenerER (equivalenciaMER (Movi a x b))
obtenerER (MoviER a e b) = e

estadosLLegada :: (Eq a,Ord a) => Cjto (Mov a) -> a -> Cjto a
estadosLLegada (Cj []) _ = (Cj [])
estadosLLegada (Cj ((Movi t _ m):xs)) n
    |t == n = unionCjto (Cj [m]) (estadosLLegada (Cj (xs)) n)
    |otherwise = (estadosLLegada (Cj (xs)) n)

estadosQueLLegan ::(Eq a,Ord a) => Cjto (Mov a) -> a -> Cjto a
estadosQueLLegan (Cj []) _ = (Cj [])
estadosQueLLegan (Cj ((Movi m _ t):xs)) n
    |t == n = anadeACjto (m) (estadosQueLLegan (Cj (xs)) n)
    |otherwise = (estadosQueLLegan (Cj (xs)) n)
    
--2.3.2. AFD (Autómata finito determinista)
data AFD a = AfdC (Cjto a) (Cjto Char) (Cjto (Mov a)) a (Cjto a)
    deriving (Eq, Ord, Show, Read)

estadosAFD :: AFD a -> Cjto a
estadosAFD (AfdC (Cj (xs)) (Cj ys) (Cj (zs)) x (Cj (ws))) = (Cj (xs))

alfabetoAFD :: AFD a -> Cjto Char
alfabetoAFD (AfdC (Cj (xs)) (Cj ys) (Cj (zs)) x (Cj (ws))) = (Cj (ys))

movimientosAFD :: AFD a -> Cjto (Mov a)
movimientosAFD (AfdC (Cj (xs)) (Cj ys) (Cj (zs)) x (Cj (ws))) = (Cj (zs))

estadoInicialAFD :: AFD a -> a
estadoInicialAFD (AfdC (Cj (xs)) (Cj ys) (Cj (zs)) x (Cj (ws))) = x

aceptacionAFD :: AFD a -> Cjto a
aceptacionAFD (AfdC (Cj (xs)) (Cj ys) (Cj (zs)) x (Cj (ws))) = (Cj (ws))

sumafd :: (Num a, Ord a) => a -> AFD a -> AFD a
sumafd n (AfdC (estados1) (alfabeto1) (mov1) x (aceptacion1)) = AfdC (estados) (alfabeto1) (mov) (x+n) (aceptacion)
    where estados = mapCjto (sumaEfd n) (estados1)
          mov = mapCjto (sumaMfd n) (mov1)
          aceptacion = mapCjto (sumaEfd n) (aceptacion1)
          
sumaEfd :: (Num a, Ord a) => a -> a -> a
sumaEfd n m = n+m

sumaMfd :: (Num a, Ord a) => a -> Mov a -> Mov a
sumaMfd n (Movi m x t) = Movi (m+n) x (t+n)

--3.ER (Expresiones Regulares)
data ER = Vacio | Epsilon | Term Char | Eleccion ER ER | Concat ER ER | Cierre ER | Parentesis ER
    deriving (Eq, Ord, Show, Read)
--4.Implementación de las funciones
--4.1. AFN-E -> AFN (Paso de ANF-E a AFN)
cierreE :: (Eq a, Ord a) => (Cjto (MovE a)) -> (Cjto (MovE a)) -> a -> (Cjto a)
cierreE (Cj []) _ x = (Cj [x])
cierreE (Cj (m:ms)) (Cj (cs)) (x)
    |((estadolibre (m)) == x) && ((conjuntolibre (m)) == (Cj [])) = (Cj [x])
    |((estadolibre (m)) == x) = (unionCjto (unionCjto (conjuntolibre (m)) (unionmulti (mapCjto (cierreE (Cj (cs)) (Cj (cs))) (conjuntolibre (m))))) (Cj [x]))
    |otherwise = (cierreE (Cj (ms)) (Cj (cs)) (x))
    
auxxfuncionepsilon :: (Eq a, Ord a) => a -> Char -> (Cjto (MovE a)) -> (Cjto a) -> (Cjto a)
auxxfuncionepsilon _ _ (Cj []) _ = (Cj [])
auxxfuncionepsilon (q) (r) (Cj (m:ms)) (Cj (ys))
    |((caracterdeMovE (m))== r) && (estaEnCjto (estadopartidanolibre (m)) (Cj (ys))) = (unionCjto (conjuntodemovsnolibres (m)) (auxxfuncionepsilon (q) (r) (Cj (ms)) (Cj (ys))))
    |otherwise = (auxxfuncionepsilon (q) (r) (Cj (ms)) (Cj (ys)))

funcionepsilon :: (Eq a, Ord a) => a -> Char -> (Cjto (MovE a)) -> (Cjto a) -> (MovN a)
funcionepsilon (q) (r) (Cj (ms)) (Cj (ys)) = (MoviN (q) (r) (auxxfuncionepsilon (q) (r) (Cj (ms)) (Cj (ys))))

deltaepsilon :: (Eq a, Ord a) => (Cjto a) -> (Cjto (MovE a)) -> Char -> (Cjto (MovN a))
deltaepsilon (Cj []) _ _ = (Cj [])
deltaepsilon (Cj (x:xs)) (Cj (ms)) (j) = (unionCjto (Cj [h]) (deltaepsilon (Cj (xs)) (Cj (ms)) (j)))
    where h = (funcionepsilon (x) (j) (transicionesnolibres (Cj (ms))) c)
          c = (cierreE (transicioneslibres (Cj (ms))) (transicioneslibres (Cj (ms))) (x))

deltaepsilondef :: (Eq a, Ord a) => (Cjto a) -> (Cjto Char) -> (Cjto (MovE a)) -> (Cjto (MovN a))
deltaepsilondef (Cj (xs)) (Cj (js)) (Cj (ms)) = (unionmulti (mapCjto (deltaepsilon (Cj (xs)) (Cj (ms))) (Cj (js))))

aceptacionNEaN :: (Eq a, Ord a) => (Cjto a) -> (Cjto (MovE a)) -> (Cjto a) -> (Cjto a)
aceptacionNEaN (Cj []) _  _ = (Cj [])
aceptacionNEaN (Cj (x:xs)) (Cj (ms)) (Cj (fs))
    |((interseccionCjto (c) (Cj (fs))) /= (Cj [])) = (unionCjto (Cj [x]) (aceptacionNEaN (Cj (xs)) (Cj (ms)) (Cj (fs))))
    |otherwise = (aceptacionNEaN (Cj (xs)) (Cj (ms)) (Cj (fs)))
        where c = (cierreE (transicioneslibres (Cj (ms))) (transicioneslibres (Cj (ms))) (x))

afneAafn :: (Eq a, Ord a) => (AFNE a) -> (AFN a)
afneAafn (AfneC (Cj (xs)) (Cj (js)) (Cj (ms)) q (Cj (fs))) = (AfnC (Cj (xs)) (Cj (js)) (movssn) q (acp))
    where movssn = (deltaepsilondef (Cj (xs)) (Cj (js)) (Cj (ms)))
          acp = (aceptacionNEaN (Cj (xs)) (Cj (ms)) (Cj (fs)))

--4.2. AFN -> AFD (Paso de AFN a AFD)
prevNaD ::Eq a => (Cjto (MovN a)) -> (Cjto (Cjto a))
prevNaD (Cj []) = (Cj [])
prevNaD (Cj (x:xs)) = (anadeACjto (d) (prevNaD (Cj (xs))))
    where d = conjuntodeaMovN (x)
    
def :: (Eq a, Ord a) => (Cjto (Char)) -> (Cjto (Cjto a)) -> (Cjto (MovN a)) -> (Cjto (Cjto a))
def _ (Cj []) _ = (Cj [])
def (Cj (js)) (Cj (x:xs)) (Cj (ys)) = (unionCjto (mapCjto (damellegada) (mapCjto (buscamovNaD (x) (Cj (ys))) (Cj (js)))) (def (Cj (js)) (Cj (xs)) (Cj (ys))))

filtraAceptacionNaD :: (Eq a, Ord a) => a -> (Cjto (Cjto a)) -> (Cjto (Cjto a))
filtraAceptacionNaD x (Cj []) = (Cj [])
filtraAceptacionNaD x (Cj (z:zs))
    |estaEnCjto x z = (anadeACjto (z) (filtraAceptacionNaD x (Cj (zs))))
    |otherwise = (filtraAceptacionNaD x (Cj (zs)))

aceptacionNaD :: (Eq a, Ord a) => (Cjto a) -> (Cjto (Cjto a)) -> (Cjto (Cjto a))
aceptacionNaD (Cj []) _ = (Cj [])
aceptacionNaD (Cj (x:xs)) (Cj (zs)) = (unionCjto (filtraAceptacionNaD (x) (Cj (zs))) (aceptacionNaD (Cj (xs)) (Cj (zs))))

buscamovNaD :: (Eq a, Ord a) => (Cjto a) -> (Cjto (MovN a)) -> Char -> (Mov (Cjto a))
buscamovNaD (Cj (xs)) (Cj []) (j) = (Movi (Cj (xs)) (j) (Cj []))
buscamovNaD (Cj (xs)) (Cj (y:ys)) (j)
    |(estaEnCjto (dameEstadopartia y) (Cj (xs))) && (j == (caracterdeMovN y)) = (Movi (Cj (xs)) (j) (unionCjto (conjuntodeaMovN y) (auxx (buscamovNaD (Cj (xs)) (Cj (ys)) (j)))))
    |otherwise = (buscamovNaD (Cj (xs)) (Cj (ys)) (j))
    
funcionTransAFD :: (Eq a, Ord a) => (Cjto (Cjto a)) -> (Cjto (Char)) -> (Cjto (MovN a)) -> (Cjto (Mov (Cjto a)))
funcionTransAFD (Cj []) _ _ = (Cj [])
funcionTransAFD (Cj (x:xs)) (Cj (js)) (Cj (ys)) = (unionCjto (mapCjto (buscamovNaD (x) (Cj (ys))) (Cj (js))) (funcionTransAFD (Cj (xs)) (Cj (js)) (Cj (ys))))

diccionario :: (Num a, Ord a, Enum a) => Cjto (Cjto a) -> Cjto a -> a
diccionario c d = snd (dic)!!n 
    where dic = diccionarioAFD (c)
          n = posicionElemLista d (fst(dic))

diccionarioAFD :: (Num a, Ord a, Enum a) => Cjto (Cjto a) -> ([Cjto a], [a])
diccionarioAFD (Cj (x:xs)) =  ((x:xs),([1..(cardinal(Cj (x:xs)))]))

posicionElemLista :: Eq a => a -> [a] -> Int
posicionElemLista x (xs) = (quitarMaybe (elemIndex x (xs)))

quitarMaybe:: Maybe a -> a
quitarMaybe x = fromJust(x)

cambiaEstadosAFD :: (Num a,Ord a,Enum a) => Cjto (Cjto a) -> Cjto a
cambiaEstadosAFD c = Cj (snd (diccionarioAFD (c)))

cambiaMovimientoAFD ::(Num a, Ord a, Enum a) => Cjto (Cjto a) -> Mov (Cjto a) -> Mov a
cambiaMovimientoAFD c (Movi x y z) = Movi (diccionario c x) y (diccionario c z)

cambiaMovimientosAFD :: (Num a, Ord a, Enum a) => Cjto (Cjto a) -> Cjto (Mov (Cjto a)) -> Cjto (Mov a)
cambiaMovimientosAFD c d = mapCjto (cambiaMovimientoAFD c) d

cambiaAFD :: (Num a, Ord a, Enum a) => AFD (Cjto a) -> AFD a
cambiaAFD (AfdC (e) (a) (m) (i) (ac)) = AfdC (cambiaEstadosAFD(e)) (a) (cambiaMovimientosAFD (e) (m)) (diccionario (e) (i)) (cambiaAceptacionAFD (ac) (e))

cambiaAceptacionAFD :: (Num a, Ord a, Enum a) => Cjto (Cjto a) -> Cjto (Cjto a) -> Cjto a
cambiaAceptacionAFD c d = mapCjto (diccionario d) c

afnAafd ::(Eq a, Ord a) => (AFN a) -> (AFD (Cjto a))
afnAafd (AfnC (Cj (xs)) (Cj (js)) (Cj (ms)) q (Cj (fs))) = (AfdC (estados) (Cj (js)) (funcionTransAFD (estados) (Cj (js)) (Cj (ms))) (Cj [q]) (aceptacionNaD (Cj (fs)) (estados)))
    where estados = unionCjto (prevNaD (Cj (ms))) (unionCjto (def (Cj (js)) (prevNaD (Cj (ms))) (Cj (ms))) (Cj [(Cj [q])]))
    
--4.3. AFN-E -> AFD (Paso de AFN-E a AFD)
afneAafd :: (Eq a, Ord a) => (AFNE a) -> (AFD (Cjto a))
afneAafd (AfneC (Cj (xs)) (Cj (js)) (Cj (ms)) q (Cj (fs))) = (afnAafd (nodet))
    where nodet = (afneAafn ((AfneC (Cj (xs)) (Cj (js)) (Cj (ms)) q (Cj (fs)))))

--4.5.Minimización de AFD (Algoritmo HopCroft)
actualizaSetP :: (Eq a,Ord a) => Cjto (Cjto a) -> Cjto (Cjto a) -> Cjto a -> Cjto (Cjto a)
actualizaSetP p (Cj []) _ = p
actualizaSetP p (Cj (y:ys)) x = actualizaSetP nuevoP (Cj ys) x
    where nuevoP = anadeACjto (diferenciaCjto y x) ((anadeACjto (interseccionCjto y x) (eliminaDeCjto y p))) 

actualizarSetW :: (Eq a,Ord a) => Cjto(Cjto a) -> Cjto(Cjto a) -> Cjto a -> Cjto (Cjto a)
actualizarSetW w (Cj []) _ = w
actualizarSetW w (Cj (y:ys)) x 
    | estaEnCjto (y) (w) = actualizarSetW (actualizaSetP w (Cj[y]) x) (Cj (ys)) x
    | cardinal(interseccionCjto y x) <= cardinal(diferenciaCjto y x) = actualizarSetW (anadeACjto (interseccionCjto y x) w) (Cj (ys)) x
    | otherwise = actualizarSetW (anadeACjto (diferenciaCjto y x) w) (Cj (ys)) x
    
hop :: (Eq a,Ord a) => (Cjto (Cjto a),  Cjto (Cjto a),  Cjto (Mov a))-> (Cjto (Cjto a),  Cjto (Cjto a),  Cjto (Mov a))
hop ((Cj []), o, movimientos) = (Cj [], o, movimientos)
hop ((Cj (w:ws)), (Cj p), movimientos) = hop ((actualizaW), (actualizaP), movimientos)
    where x = conjuntoX movimientos w
          y = filterCjto (f x) (Cj p)
          f set1 set2 = (not(esCjVacio (interseccionCjto set1 set2))) && (not(esCjVacio (diferenciaCjto set2 set1)))
          actualizaP = actualizaSetP (Cj p) y  x
          actualizaW = actualizarSetW (Cj ws) y x 

conjuntoX:: (Eq a, Ord a) => Cjto (Mov a) -> Cjto a -> Cjto a
conjuntoX _ (Cj []) = Cj []
conjuntoX movimientos w = foldl1Cjto unionCjto (mapCjto (estadosQueLLegan movimientos) (w))


susti ::  (Eq a, Ord a) => Cjto(Cjto a) -> Mov a -> Mov a
susti  (set) (Movi n l m)= Movi x l y
    where x = primerElemento (primerElemento (filterCjto (estaEnCjto n) set))
          y = primerElemento (primerElemento (filterCjto (estaEnCjto m) set))
          
reducirAFD :: (Eq a, Ord a) => AFD a -> AFD a
reducirAFD (AfdC (q) (alfabeto) (movimientos) (inicial) (finales)) = (AfdC (diferenciaCjto nuevoQ inalcanzables) (alfabeto) (nuevoMovis) nuevoInicial (nuevosFinales))
    where set1 =filterCjto (noEsCjVacio) (Cj[(diferenciaCjto q finales), finales])
          (w, p, mov) = hop (set1, set1 , movimientos)
          nuevoQ = mapCjto (primerElemento) p
          nuevoMovis = filterCjto (nosaleDe inalcanzables) (eliminaRepeticionesCjto (mapCjto (susti p) (movimientos)))
          nuevosFinales = filterCjto ((flip estaEnCjto) (diferenciaCjto nuevoQ inalcanzables)) (finales)
          nuevoInicial = primerElemento (primerElemento (filterCjto (estaEnCjto inicial) p))
          inalcanzables = estadosInalcanzables (nuevoQ) (movimientos) nuevoInicial
          
          
estadosInalcanzables :: (Eq a, Ord a) => Cjto a -> Cjto (Mov a) -> a -> Cjto a
estadosInalcanzables estados movimientos inicial = filterCjto (f movimientos) estadosN
    where f x y = esCjVacio(estadosQueLLegan x y)
          estadosN = diferenciaCjto (estados) (Cj [inicial])
          
          
nosaleDe :: (Eq a, Ord a) => Cjto a -> Mov a -> Bool
nosaleDe (Cj (x:xs)) (Movi y l z) = (x /= y) && nosaleDe (Cj xs) (Movi y l z)
nosaleDe (Cj []) _ = True

 

--4.6. Equivalencia entre dos AFD's
sonEquivalentes :: (Num a, Ord a) => AFD a -> AFD a -> Bool 
sonEquivalentes (AfdC (q1) (_) (movimientos1) (inicial1) (finales1)) (AfdC (q2) (_) (movimientos2) (inicial2) (finales2)) = (filterCjto (estaEnCjto inicial1) (p)) == (filterCjto (estaEnCjto inicial2) (p))
    where q = unionCjto q1 q2
          movimientos = unionCjto movimientos1 movimientos2
          finales = unionCjto finales1 finales2
          set1 =filterCjto (noEsCjVacio) (Cj[(diferenciaCjto q finales), finales])
          (w, p, mov) = hop (set1, set1 , movimientos)      

equivalenciaAFD :: (Num a, Ord a) => AFD a -> AFD a -> Bool
equivalenciaAFD afd1 afd2 
    | esCjVacio (interseccionCjto (estadosAFD afd1) (estadosAFD afd2)) = sonEquivalentes afd1 afd2
    | otherwise = sonEquivalentes (afd1) (sumafd (maximoCjto (estadosAFD afd1)) afd2)
    
--4.7.Paso de ER a AFN-E
erAFNE :: (Num a, Ord a) => ER -> AFNE a
erAFNE er = AfneC (estadosAFNE (afne)) (alfabetoAFNE (afne)) (ordenarMovE (estadosAFNE(afne)) (movimientosAFNE (afne))) (estadoInicialAFNE(afne)) (aceptacionAFNE(afne))
    where afne = aux2 (er)
    
aux2 :: (Num a, Ord a) => ER -> AFNE a
aux2 (Vacio) = AfneC (Cj [0]) (Cj []) (Cj []) 0 (Cj [])
aux2 (Epsilon) = AfneC (Cj[0]) (Cj []) (Cj []) 0 (Cj [0])
aux2 (Term x) = AfneC (Cj [0,1]) (Cj [x]) (Cj [MoviE 0 x (Cj [1])]) 0 (Cj [1])
aux2 (Concat x y) = concatAFNE (aux2 x) (aux2 y)
aux2 (Eleccion x y) = eleccionAFNE (aux2 x) (aux2 y)
aux2 (Cierre x) = cierreAFNE (aux2 x)

eleccionAFNE :: (Num a, Ord a) => AFNE a -> AFNE a -> AFNE a
eleccionAFNE (afne1) (afne2) = unionEleccionAFNE (suma 1 afne1) (suma (m1+1) afne2)
    where m1 = numEstadosAFNE (afne1)

unionEleccionAFNE :: (Num a, Ord a) => AFNE a -> AFNE a -> AFNE a
unionEleccionAFNE (AfneC (estados1) (alfabeto1) (mov1) x (aceptacion1)) (AfneC (estados2) (alfabeto2) (mov2) y (aceptacion2)) = AfneC (unionCjto (unionCjto (Cj [0]) (estados1)) (estados2))(unionCjto (alfabeto1) (alfabeto2)) (unionCjto (nuevomovs) (unionCjto (mov1) (mov2))) 0 (unionCjto (aceptacion1) (aceptacion2))
    where nuevomovs = Cj [EmoviE 0 (Cj [x,y])]

concatAFNE :: (Num a,Ord a) => AFNE a -> AFNE a -> AFNE a
concatAFNE (afne1) (afne2) = unionConcatAFNE (afne1) (suma m1 afne2)
    where m1 = numEstadosAFNE (afne1)

unionConcatAFNE :: (Num a, Ord a) => AFNE a -> AFNE a -> AFNE a
unionConcatAFNE (AfneC (e1) (a1) (mov1) x (ac1)) (AfneC (e2) (a2) (mov2) y (ac2)) = AfneC (unionCjto (e1) (e2)) (unionCjto (a1) (a2)) (unionCjto (simplificarMovAFNE (e) (m)) (transicionesnolibres m)) x (ac2)
    where m = unionCjto(unionCjto (mapCjto (nuevosMov y) (ac1)) (mov1)) (mov2)
          e = unionCjto (e1) (e2)

cierreAFNE :: (Num a, Ord a) => AFNE a -> AFNE a
cierreAFNE (afne1) = unionCierreAFNE (suma 1 afne1)

unionCierreAFNE :: (Num a,Ord a) => AFNE a -> AFNE a
unionCierreAFNE (AfneC (e) (a) (m) x (ac)) = AfneC (unionCjto (Cj [0]) (e)) (a) (unionCjto (mov) (m)) 0 (Cj [0])
    where mov = unionCjto (mapCjto (nuevosMov 0) (ac)) (Cj [EmoviE 0 (Cj [1])])

simplificarMovAFNE :: (Num a, Ord a) =>  Cjto a -> Cjto (MovE a) -> Cjto (MovE a)
simplificarMovAFNE (Cj []) (ms) = (Cj [])
simplificarMovAFNE (Cj (e:es)) (ms) = unionCjto (unificacionMovAFNE e (ms)) (simplificarMovAFNE (Cj (es)) (ms))

unificarMovAFNE :: (Num a, Ord a) => Cjto (MovE a) -> Cjto (MovE a)
unificarMovAFNE (Cj []) = Cj []
unificarMovAFNE (Cj (m:ms)) = (Cj [EmoviE (estadolibre (m))  (unionmulti (mapCjto (conjuntolibre) (Cj (m:ms))))])

unificacionMovAFNE :: (Num a, Ord a) => a -> Cjto (MovE a) -> Cjto (MovE a)
unificacionMovAFNE x ms = unificarMovAFNE (filterCjto (filtro x) (transicioneslibres(ms)))

filtro ::(Num a, Ord a) => a -> MovE a -> Bool
filtro x m = if x == estadolibre(m) then True else False

--4.8.Paso de AFD a ER
simplificarER :: ER -> ER
simplificarER (Epsilon) = (Epsilon)
simplificarER (Vacio) = Vacio
simplificarER (Term x) = (Term x)
simplificarER (Eleccion (Vacio) e) = (simplificarER e)
simplificarER (Eleccion e (Vacio)) = (simplificarER e)
simplificarER (Concat (Vacio) e) = Vacio
simplificarER (Concat e (Vacio)) = Vacio
simplificarER (Concat (Epsilon) e) = (simplificarER e)
simplificarER (Concat e (Epsilon)) = (simplificarER e)
simplificarER (Cierre (Vacio)) = Epsilon
simplificarER (Cierre (Epsilon)) = Epsilon
simplificarER (Cierre (Eleccion (Epsilon) e)) = (Cierre e)
simplificarER (Cierre (Eleccion e (Epsilon))) = (Cierre e)
simplificarER (Eleccion e1 e2) = if e1 == e2 then (simplificarER e1) else Eleccion (simplificarER e1) (simplificarER e2)
simplificarER (Concat e1 e2) =  Concat (simplificarER e1) (simplificarER e2)
simplificarER (Cierre (Cierre e)) = Cierre (simplificarER e)
simplificarER (Cierre e) = (Cierre (simplificarER e))

afdER :: (Num a, Ord a) => AFD a -> ER
afdER (AfdC (estados) (alfabeto) (movimientos) x (aceptacion)) = simplificarER (simplificarER (unionER (mapCjto (eIJK x m (movimientos)) (aceptacion))))
    where m = cardinal (estados)

hayMov ::(Num a, Ord a) => a -> a -> Cjto (Mov a) -> Bool
hayMov p q (Cj []) = False
hayMov p q (Cj (m:ms))
    |(estadoEntrada p m) && (estadoSalidaFD q m) = True
    |otherwise = (hayMov p q (Cj (ms)))


eIJK :: (Num a, Ord a) => a -> a -> Cjto (Mov a) -> a -> ER
eIJK i 0 (Cj (ms)) j
    |i /= j && (hayMov i j (Cj (ms))) = simplificarER (unionER (mapCjto (obtenerER) (interseccionCjto (estadosEntrada i (Cj (ms))) (estadosSalida j (Cj (ms))))))
    |i == j && (hayMov i j (Cj (ms))) = simplificarER (Eleccion (Epsilon) (unionER (mapCjto (obtenerER) (interseccionCjto (estadosEntrada i (Cj (ms))) (estadosSalida j (Cj (ms)))))))
    |i == j && not (hayMov i j (Cj (ms))) = Epsilon
    |otherwise = Vacio
eIJK i k (Cj (ms)) j = simplificarER (Eleccion (eIJK i (k-1) (Cj (ms)) j) (Concat (eIJK i (k-1) (Cj(ms)) k) (Concat (Cierre (eIJK k (k-1) (Cj (ms)) k)) (eIJK k (k-1) (Cj (ms)) j))))

unionER :: Cjto (ER) -> ER
unionER (Cj []) = Vacio
unionER (Cj [x]) = x
unionER (Cj (x:xs)) = simplificarER (Eleccion (x) (unionER (Cj (xs))))
    
estadoEntrada ::(Num a,Ord a) => a -> Mov a -> Bool
estadoEntrada x (Movi w y z) = (x == w)
estadoEntrada x (MoviER w y z) = (x == w)

estadosEntrada :: (Num a, Ord a) => a -> Cjto (Mov a) -> Cjto (Mov a)
estadosEntrada x (Cj (xs)) = Cj (filter (estadoEntrada x) (xs))

estadoSalidaFD ::(Num a,Ord a) => a -> Mov a -> Bool
estadoSalidaFD x (Movi w y z) = (x == z)
estadoSalidaFD x (MoviER w y z) = (x == z)

estadosSalida :: (Num a, Ord a) => a -> Cjto (Mov a) -> Cjto (Mov a)
estadosSalida x (Cj (xs)) = Cj (filter (estadoSalidaFD x) (xs))

--4.9.Reconocimiento de cadenas vía AFD
reconoce :: (Num a, Eq a) => String -> AFD a -> Bool
reconoce palabra afd = if (patata (palabra) (alfabetoAFD afd)) then estaEnCjto llego (aceptacionAFD afd) else False
    where llego = movimiento inicial movimientos palabra
          movimientos = movimientosAFD afd
          inicial = estadoInicialAFD afd

movimiento ::(Num a, Eq a) => a -> Cjto (Mov a) -> [Char] -> a
movimiento n _  [] = n
movimiento n set (x:xs) = movimiento (s) set (xs)
    where s = paso n set  x

paso :: (Num a, Eq a) => a -> Cjto (Mov a) -> Char -> a
paso n (Cj ((Movi x l y):ms)) r 
    | x == n && l ==r = y
    | otherwise = paso n (Cj ms) r

patata :: String ->  Cjto (Char) -> Bool
patata [] (c) = True
patata (p:ps) (c)
    | estaEnCjto p c = patata (ps) (c)
    | otherwise = False

--5. Interfaz (Entrada/Salida)
leeInt :: IO Int
leeInt = do linea <- getLine
            return (read linea)

leeIntRango :: Int -> Int -> IO Int
leeIntRango  men may = do putStr ("Introduce numero entre " ++ show men ++ " y " ++ show may ++ ": ")
                          n <- leeInt
                          if  (n>may)||(n<men) then leeIntRango men may else return n
                          
menu :: IO()
menu = do putStr    "1:Pasa de AFN-E a AFN\n2:Pasa de AFN a AFD\n3:Pasa de AFN-E a AFD\n4:Pasar de AFD a ER\n5:Pasar de ER a AFN-E\n6:Minimizar AFD\n7:Equivalencia entre AFDs\n8:Reconocimiento cadenas via AFD\n9:Pasar de ER a AFN\n10:Pasar de ER a AFD\n11:Reconocimiento cadenas via ER\n12:Automata minimo asociado a una ER\n13:Terminar\n"
          opcion <- leeIntRango 1 13
          case opcion of
            1 -> do interfaz1
                    menu
            2 -> do interfaz2
                    menu
            3 -> do interfaz3
                    menu
            4 -> do interfaz4
                    menu
            5 -> do interfaz5
                    menu
            6 -> do interfaz6
                    menu
            7 -> do interfaz7
                    menu
            8 -> do interfaz8
                    menu
            9 -> do interfaz9
                    menu
            10 -> do interfaz10
                     menu
            11 -> do interfaz11
                     menu
            12 -> do interfaz12
                     menu
            13 -> putStr "Hasta luego"
           
interfaz1 :: IO()
interfaz1 = do putStr "Dime el nombre del fichero de entrada: "
               fichero <- getLine
               datos <- readFile fichero
               let afntrans = 0 `suma` (read (datos))
                   afn = (afneAafn (afntrans))
               putStr "Dime el nombre del fichero de salida: "
               ficheroSalida <- getLine
               writeFile ficheroSalida (show (afn))
               
interfaz2 :: IO()
interfaz2 = do putStr "Dime el nombre del fichero de entrada: "
               fichero <- getLine
               datos <- readFile fichero
               let afn = 0 `sumaFN` (read (datos))
                   afd = cambiaAFD (afnAafd (afn))
               putStr "Dime el nombre del fichero de salida: "
               ficheroSalida <- getLine
               writeFile ficheroSalida (show (afd))
               
interfaz3 :: IO()
interfaz3 = do putStr "Dime el nombre del fichero de entrada: "
               fichero <- getLine
               datos <- readFile fichero
               let afntrans = 0 `suma` (read (datos))
                   afd = cambiaAFD (afneAafd (afntrans))
               putStr "Dime el nombre del fichero de salida: "
               ficheroSalida <- getLine
               writeFile ficheroSalida (show (afd))

interfaz4 :: IO()
interfaz4 = do putStr "Dime nombre del fichero entrada: "
               fichero <- getLine
               datos <- readFile fichero
               let resultado = afdER (sumafd 0 (read datos))
               putStr "Dime nombre del fichero salida: "
               ficheroSalida <- getLine
               writeFile ficheroSalida (show (resultado))

interfaz5 :: IO()
interfaz5 = do putStr "Dime el nombre del fichero de entrada: "
               fichero <- getLine
               datos <- readFile fichero
               let resultado = erAFNE (simplificarER (Eleccion (read (datos)) Vacio))
               putStr "Dime nombre del fichero salida: "
               ficheroSalida <- getLine
               writeFile ficheroSalida (show (resultado))
               
interfaz6 :: IO()
interfaz6 = do putStr "Dime el nombre del fichero de entrada: "
               fichero <- getLine
               datos <- readFile fichero
               let afd = 0 `sumafd` (read datos)
                   afdMinimo = reducirAFD afd
               putStr "Dime nombre del fichero de salida: "
               ficheroSalida <- getLine
               writeFile ficheroSalida (show (afdMinimo))
             
interfaz7 :: IO()
interfaz7 = do putStr "Dime el nombre del fichero de entrada: "
               fichero <- getLine
               datos <- readFile fichero
               let [afd1str, afd2str] = (lines (datos))
                   afd1 = 0 `sumafd` (read afd1str)
                   afd2 = 0 `sumafd` (read afd2str)
                   respuesta = (equivalenciaAFD (afd1) (afd2))
               print respuesta
             
interfaz8 :: IO()
interfaz8 = do putStr "Dime el nombre del fichero de entrada: "
               fichero <- getLine
               datos <- readFile fichero
               putStr "Dime la cadena a reconocer: "
               cadena <- getLine
               let resultado = reconoce (cadena) (sumafd 0 (read datos))
               print resultado

--Pasar de ER a AFN    
interfaz9 :: IO()
interfaz9 = do putStr "Dime el nombre del fichero de entrada: "
               fichero <- getLine
               datos <- readFile fichero
               let afnepaso = erAFNE (simplificarER (Eleccion (read (datos)) Vacio))
                   resultado = (afneAafn (afnepaso))
               putStr "Dime el nombre del fichero de salida: "
               ficheroSalida <- getLine
               writeFile ficheroSalida (show (resultado))

--Pasar de ER a AFD
interfaz10 :: IO()
interfaz10 = do putStr "Dime el nombre del fichero de entrada: "
                fichero <- getLine
                datos <- readFile fichero
                let afnepaso = erAFNE (simplificarER (Eleccion (read (datos)) Vacio))
                    resultado = cambiaAFD (afneAafd (afnepaso))
                putStr "Dime el nombre del fichero de salida: "
                ficheroSalida <- getLine
                writeFile ficheroSalida (show (resultado))

interfaz11 :: IO()
interfaz11 = do putStr "Dime el nombre del fichero de entrada: "
                fichero <- getLine
                datos <- readFile fichero
                putStr "Dime la cadena a reconocer: "
                cadena <- getLine
                let afnepaso = erAFNE (simplificarER (Eleccion (read (datos)) Vacio))
                    afdpaso = cambiaAFD (afneAafd (afnepaso))
                    resultado = (reconoce (cadena) (afdpaso))
                print resultado

--Automata minimo del lenguaje asociado a una ER (si Alberto consigue la funcion que quiere hacer)
interfaz12 :: IO()
interfaz12 = do putStr "Dime el nombre del fichero de entrada: "
                fichero <- getLine
                datos <- readFile fichero
                let afnepaso = erAFNE (simplificarER (Eleccion (read (datos)) Vacio))
                    afdpaso = cambiaAFD (afneAafd (afnepaso))
                    afdminimo = (reducirAFD (afdpaso))
                putStr "Dime el nombre del fichero de salida: "
                ficheroSalida <- getLine
                writeFile ficheroSalida (show (afdminimo))
              