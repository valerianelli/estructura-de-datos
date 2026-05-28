-- Tema de la clase anterior:
    -- Tipos algebraicos
       -- enumerativos, Ej: Dir
       -- registrosm Ej: Persona
    -- Funciones y tipos
    -- Pattern Matching
    -- Polimorfismo (paramétrico)

sumarDos x = x + 2

-- asi definimos un enumerativo
data Dir = Norte | Sur | Este | Oeste

-- -- una función total
-- f :: Dir -> ...
-- f Norte = ...
-- f Sur = ...
-- f Este = ...
-- f Oeste = ...

-- -- una función parcial
-- f :: Dir -> ...
-- f Norte = ...
-- f Sur = ...
-- f Este = ...

-- una función total
esNorte :: Dir -> Bool
esNorte Norte = True
esNorte x     = False

loMismo :: a -> a
loMismo x = x

siempreTrue :: a -> Bool
siempreTrue x = True

---------------------------------------------

-- Clase de hoy

null :: [a] -> Bool
null [] = True
null (x:xs) = False

null' :: [a] -> Bool
null' [] = True
null' xs = False

head' :: [a] -> a
head' [] = error "no tiene ningun elemento"
head' (x:xs) = x

-- data [a] = [] | a : [a]

tail' :: [a] -> [a]
tail' [] = error "no tiene ningun elemento"
tail' (x:xs) = xs

-- concatenar :: [a] -> [a] -> [a]
-- concatenar [] ys = ...
-- concatenar (x:xs) ys = ...

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

-- sumatoria []
-- ->
-- 0

-- sumatoria (1:(2:(3:[])))
-- -> def sumatoria, x = 1, xs = (2:(3:[]))
-- 1 + sumatoria (2:(3:[]))
-- -> def sumatoria, x = 2, xs = (3:[])
-- 1 + (2 + sumatoria (3:[]))
-- -> def sumatoria, x = 3, xs = []
-- 1 + (2 + (3 + sumatoria []))
-- -> def sumatoria, para lista []
-- 1 + (2 + (3 + 0))
-- ->
-- 6

-- función polimórfica
-- es una función total
longitud :: [a] -> Int
longitud [] =  0
longitud (x:xs) = 1 + longitud xs

-- longitud (1:2:3:[])
-- ->
-- 1 + longitud (2:3:[])
-- ->
-- 1 + 1 + longitud (3:[])
-- ->
-- 1 + 1 + 1 + longitud []
-- ->
-- 1 + 1 + 1 + 0
-- ->
-- 3

productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs

todoTrue :: [Bool] -> Bool
todoTrue [] = True
todoTrue (b:bs) = b && todoTrue bs

-- todoTrue (True:True:True:[])
-- ->
-- ...
-- ->
-- True && True && True && True
-- ->
-- ...
-- ->
-- True

algunoTrue :: [Bool] -> Bool
algunoTrue [] = False
algunoTrue (b:bs) = b || algunoTrue bs

-- algunoTrue (True:True:True:[])
-- ->
-- ...
-- ->
-- True || True || True || True
-- ->
-- ...
-- ->
-- True

-- algunoTrue (False:False:False:[])
-- ->
-- ...
-- ->
-- False || False || False || True
-- ->
-- ...
-- ->
-- False

porDos :: [Int] -> [Int]
porDos [] = []
porDos (x:xs) = 
    x * 2 : porDos xs

-- porDos (1:2:3:[])
-- ->
-- ...
-- ->
-- (1*2) : (2*2) : (2*3) : []
-- ->
-- ...
-- ->
-- (2:4:6:[])

hayAlMenosUn5 :: [Int] -> Bool
hayAlMenosUn5 [] = False
hayAlMenosUn5 (x:xs) = 
    x == 5 || hayAlMenosUn5 xs

hayAlMenosUnN :: Int -> [Int] -> Bool
hayAlMenosUnN n [] = False
hayAlMenosUnN n (x:xs) = 
    x == n || hayAlMenosUnN n xs

soloLosMayoresA :: Int -> [Int] -> [Int]
soloLosMayoresA n [] = []
soloLosMayoresA n (x:xs) =
    if x > n
       then x : soloLosMayoresA n xs
       else soloLosMayoresA n xs

aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (x:xs) = x ++ aplanar xs

conLongitudMayorA :: Int -> [[a]] -> [[a]]
conLongitudMayorA n [] = []
conLongitudMayorA n (x:xs) =
    if longitud x > n
       then x : conLongitudMayorA n xs
       else conLongitudMayorA n xs

-- función parcial
elMaximo :: [Int] -> Int
elMaximo [] = error "no tiene maximo"
elMaximo (x:[]) = x
elMaximo (x:xs) = 
    max x (elMaximo xs)

-- similar
-- head [] = error ""
-- head (x:xs) = x

-- max :: Int -> Int -> Int
-- max x y =
--     if x > y
--        then x
--        else y

-- sería incorrecto porque el cero
-- no pertenece a la lista
-- elMaximo' :: [Int] -> Int
-- elMaximo' [] = 0
-- elMaximo' (x:xs) = 
--     max x (elMaximo' xs)

factorial 0 = 1
factorial n =
    n * factorial (n - 1)

-- factorial 3 
-- ->
-- 3 * factorial 2
-- ->
-- 3 * 2 * factorial 1
-- ->
-- 3 * 2 * 1 * factorial 0
-- ->
-- 3 * 2 * 1 * 1
-- ->
-- 6

losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 []     = []
losPrimeros 0 (x:xs) = []
losPrimeros n [] = []
losPrimeros n (x:xs) =
    x : losPrimeros (n - 1) xs

losPrimeros' :: Int -> [a] -> [a]
losPrimeros' 0 xs     = []
losPrimeros' n []     = []
losPrimeros' n (x:xs) =
    x : losPrimeros' (n - 1) xs

data Persona = ConsP String Int deriving Show

edad :: Persona -> Int
edad (ConsP nom ed) = ed

edades :: [Persona] -> [Int]
edades [] = []
edades (p:ps) = 
    edad p : edades ps

jorgitos :: [Persona]
jorgitos = consConEdades "Jorgito" [1 .. 100]

consConEdades :: String -> [Int] -> [Persona]
consConEdades n [] = []
consConEdades n (x:xs) =
    ConsP n x : consConEdades n xs

test1 :: Bool
test1 = edades jorgitos == [1 .. 100]

