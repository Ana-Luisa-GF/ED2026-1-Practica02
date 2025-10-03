module Practica02 where
--Tipos--

data Bit = O | I 
        deriving (Show, Eq)

type Binario = [Bit]

type ListaPar a b = [(a,b)]


--Funcines auxiliares 

-- Función que calcula la longitud de una lista
-- Se usa en: 
--   - ejercicio 1 de BINARIOS (toDecimal)  
--   - ejercicio 1 de LISTAS DE LONGITUD PAR (longitud)
long :: [a] -> Int
long [] = 0
long (x:xs) = 1+(long xs)

-- Función para concatenar 2 listas
-- Se usa en:
--   - reve
--   - conjuntoPotencia
append :: [a] -> [a] -> [a]
append [] xs = xs
append (x:xs) ys = x : append xs ys

-- Función para voltear una lista
-- Se usa en:
--   - toBin
--   - suma
reve :: [a] -> [a]
reve [] = []
reve (x:xs) = append (reve xs) [x]

-- Función que transforma un número en base 10 a binario pero volteado 
-- Se usa en:
--   - toBin
toBinAux :: Int -> Binario
toBinAux 0 = []
toBinAux x = if (x `mod` 2) == 1 then I : toBinAux (x `div` 2) else O : toBinAux (x `div` 2)

-- Función que "suma 1" a un número binario
-- Se usa en:
--   - sumaAux
inc :: Binario -> Binario
inc [] = [I]
inc (O:xs) = (I:xs)
inc (I:xs) = O:(inc xs)

-- Función que hace la suma de 2 binarios pero volteados 
-- Se usa en:
--   - suma
sumaAux :: Binario -> Binario -> Binario
sumaAux xs [] = xs
sumaAux [] as = as
sumaAux (O:xs) (O:as) = O:(sumaAux xs as)
sumaAux (O:xs) (I:as) = I:(sumaAux xs as)
sumaAux (O:xs) (I:as) = I:(sumaAux xs as)
sumaAux (I:xs) (O:as) = I:(sumaAux xs as)
sumaAux (I:xs) (I:as) = O:inc(sumaAux xs as) 

-- Función que verifica si un elemento pertenece a una lista
-- Se usa en:
--   - diferenciaSimetrica
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece a (x:xs) = if x == a then True else (pertenece a xs) 

-- Función que suma las entradas "a" de una lista de pares (a,b)
-- Se usa en:
--   - sumaPares
entradaA :: (Num a, Num b) => ListaPar a b -> a
entradaA [] = 0
entradaA ((a,b):xs) = a + (entradaA xs)

-- Función que suma las entradas "b" de una lista de pares (a,b)
-- Se usa en:
--   - sumaPares
entradaB :: (Num a, Num b) => ListaPar a b -> b
entradaB [] = 0
entradaB ((a,b):xs) = b + (entradaB xs)


--BINARIOS

toDecimal :: Binario -> Int
toDecimal [] = 0
toDecimal [O] = 0
toDecimal [I] = 1
toDecimal (O:xs) = 0*(2^(long(O:xs)-1))+(toDecimal xs)
toDecimal (I:xs) = 1*(2^(long(O:xs)-1))+(toDecimal xs)

toBin :: Int -> Binario
toBin x = reve (toBinAux x)

suma :: Binario -> Binario -> Binario
suma x a = reve (sumaAux (reve x) (reve a))

--LISTAS

palindromo :: Eq a => [a] -> Bool
palindromo xs = xs == reverse xs


--Funcion principal que calcula la diferencia simetrica (Aquellos elementos que esten en la union pero no en la interseccion)
diferenciaSimetrica :: Eq a => [a] -> [a] -> [a]
diferenciaSimetrica x y = [a| a <- (x ++ y), ((pertenece a x == True) && (pertenece a y == False)) ||  ((pertenece a x == False) && (pertenece a y == True))]

--Conjunto potencia
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia [] = [[]]
conjuntoPotencia (x:xs) = append([(x:ys)|ys <- conjuntoPotencia xs]) (conjuntoPotencia xs)

--LISTAS DE LONGITUD PAR
--Esta va de regalo

--Longitud
longitud :: ListaPar a b -> Int
longitud x = (long x)*2

--Map
myMap :: (a -> c) -> (b -> d) -> ListaPar a b -> ListaPar c d 
myMap _ _ [] = []
myMap f g ((a,b):xs) = (f a, g b):(myMap f g xs)


--Sumar pares  
sumaPares :: (Num a, Num b) => ListaPar a b -> (a,b)
sumaPares [] = (0,0)
sumaPares ((a,b):xs) = ((entradaA ((a,b):xs)), (entradaB ((a,b):xs)))

--Filter pares
myFilter ::  ((a,b) -> Bool) -> ListaPar a b -> ListaPar a b
myFilter _ [] = []
myFilter f ((a,b):xs) = if f (a,b) then (a,b):(myFilter f xs) else (myFilter f xs) 