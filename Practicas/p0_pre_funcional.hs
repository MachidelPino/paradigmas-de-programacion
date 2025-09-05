-- Practica 0 --

-- Ejercicio 1 --
-- null :: Foldable t => t a -> Bool                -- Devuelve True si el valor de nulo
-- head :: [a] -> a                                 -- Devuelve la cabeza de la lista
-- tail :: [a] -> [a]                               -- Devuelve la cola de la lista
-- init :: [a] -> [a]                               -- Devuelve la lista sin su ulitmo elemento
-- last :: [a] -> a                                 -- Devuelve el ultimo elemento de la lista
-- take :: Int -> [a] -> [a]                        -- Devuelve una lista con los primeros n elementos
-- drop :: Int -> [a] -> [a]                        -- Devuelve una lista con los ultimos n elementos
-- (++) :: [a] -> [a] -> [a]                        -- Concatena dos listas
-- concat :: Foldable t => t [a] -> [a]             -- Concatena todas las sublistas
-- reverse :: [a] -> [a]                            -- Invierte el orden de la lista
-- elem :: (Foldable t, Eq a) => a -> t a -> Bool   -- Devuelve True si el elemento pertence a la lista

-- Ejercicio 2 --
-- a --
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldl" #-}
valorAbsoluto :: Float -> Float
valorAbsoluto = abs

-- b --
bisiesto :: Int -> Bool
bisiesto n | mod n 4 == 0 && (mod n 400 == 0 || mod n 100 /= 0) = True
            | otherwise = False

-- c --
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- d --
cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos n = length [d | d <- [2 .. n], esPrimo d, mod n d == 0]

esPrimo :: Int -> Bool
esPrimo p = null ([d | d <- [2..p-1], mod p d == 0])

-- Ejercicio 3 --
-- data Maybe a = Nothing | Just a
-- data Either a b = Left a | Right b

-- a --
inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso x = Just (1/x)

-- b --
aEntero :: Either Int Bool -> Int
aEntero (Left a) = a
aEntero (Right b) = if b then 1 else 0

-- Ejercicio 4 --
-- a --
limpiar :: String -> String -> String
-- Se puede usar foldl
limpiar [] ys = ys
limpiar (x:xs) ys = limpiar xs [y | y <- ys, x /= y]

-- b --
difPromedio :: [Float] -> [Float]
difPromedio xs = map (+(-(sum xs / fromIntegral (length xs)))) xs
  --where promedio = sum xs / fromIntegral (length xs)

-- c --
todosIguales :: [Int] -> Bool
todosIguales (x:xs) = length (x:xs) <= 1 || x == head xs && todosIguales xs

-- Ejercicio 5 --
data AB a = Nil | Bin (AB a) a (AB a) deriving (Show)

-- a --
vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _ = False

-- b --
negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin izq n der) = Bin (negacionAB izq) (not n) (negacionAB der)

-- c --
productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin izq n der) = n * productoAB izq * productoAB der