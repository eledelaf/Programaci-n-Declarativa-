import Distribution.Simple.Utils (xargs)
import Distribution.SPDX.LicenseId (LicenseId(CDLA_Permissive_1_0))
import Data.List ()

-- HOJA 2
-- Ejercicio 1
ej1a :: Int -> [Int]
ej1a n
    | n < 10    = [n]
    | otherwise = n`mod`10 : ej1a (n`div`10)
    
ej1aa n = reverse (ej1a n)

ej1b n = foldl f 0 (ej1a n)
    where f x y = 10*x+y
    
-- Ejercicio 2
data Natural = Cero | SUC Natural 
  deriving (Eq,Ord,Show,Read)

suma:: Natural -> Natural -> Natural
suma Cero x   = x
suma (SUC x) y = suma x (SUC y)

resta:: Natural -> Natural -> Natural
resta Cero y = Cero
resta x Cero   = x
resta (SUC x) (SUC y) = resta x y

producto :: Natural -> Natural -> Natural 
producto (SUC Cero) x = x
producto x (SUC Cero) = x
producto Cero x = Cero
producto x Cero = Cero
producto (SUC x) y =  suma y (producto x y)

fact :: Natural -> Natural
fact Cero = SUC Cero
fact (SUC x) = producto (SUC x) (fact x)

elevar :: Natural -> Natural -> Natural 
elevar x Cero = SUC Cero
elevar Cero x = Cero 
elevar x (SUC y) = producto x (elevar x y)

--Ejercicio 3
data Temp = Kelvin Float | Celsius Float | Fahrenheit Float 
 
