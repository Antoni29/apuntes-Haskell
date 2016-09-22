import Data.Char

--1. Dado un String con un numero binario, obtener una lista con los
--   bits en orden inverso
toNums bin = reverse (map (\x -> digitToInt x) bin)

--2. Obtener las primeras 32 potencias de 2
pots = [ 2^x | x <- [0..32]]
pots2 = map (2^) [0..32]

--3. Usando las funciones anteriores, onvertir a decimal a traves de
--   el produto escalar de dos listas
decimal bin = sum (zipWith (*) (toNums bin) pots)

-- Todo en una funcion -------------------------------------------------
dec bin = sum (zipWith (*) (reverse (map (\x -> digitToInt x) bin)) [2^x | x <- [0..32]])