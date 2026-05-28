-- Tipos algebraicos
-- Tipos recursivos
-- Polimorfismo

-- data [a] = [] | a : [a]

-- Recursión estructural
-- f [] = ...
-- f (x:xs) = ... f xs

-- fold
sumatoria :: [Int] -> Int -- Tipos
sumatoria []     = 0
sumatoria (x:xs) = x + sumatoria xs

-- map
sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (x:xs) = x + 1 : sucesores xs

dobles :: [Int] -> [Int]
dobles [] = []
dobles (x:xs) = x * 2 : dobles xs

-- filter
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA n [] = []
losMenoresA n (x:xs) =
	if x < n
	   then x : losMenoresA n xs
	   else losMenoresA n xs

losIguales :: Int -> [Int] -> [Int]
losIguales n [] = []
losIguales n (x:xs) =
	if x == n
	   then x : losIguales n xs
	   else losIguales n xs

losPares :: [Int] -> [Int]
losPares [] = []
losPares (x:xs) =
	if mod x 2 == 0
	   then x : losPares xs
	   else losPares xs

-- data Ordering = LT | GT | EQ

compare :: Int -> Int -> Ordering
compare n m =
	if n == m
	   then EQ
	   else if n < m
	   	       then LT
	   	       else GT

----------------------------

pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False
pertenece n (x:xs) =
	n == x || pertenece n xs

apariciones :: Eq a => a -> [a] -> Int
apariciones n [] = 0
apariciones n (x:xs) =
	if n == x
	   then 1 + apariciones n xs
	   else apariciones n xs

-----------------------------

indexar :: [a] -> [(Int, a)]
indexar []     = []
indexar (x:xs) =
	(0, x) : sumarUnoFst (indexar xs)

sumarUnoFst :: [(Int, a)] -> [(Int, a)]
sumarUnoFst [] = []
sumarUnoFst (x:xs) =
	(fst x + 1, snd x) : sumarUnoFst xs

-- indexar ["casa", "hola", "chau"]
-- ->
-- [(0, "casa"), (1, "hola"), (2, "chau")]

--------------------------------

ordenar :: [Int] -> [Int]
ordenar []     = []
ordenar (x:xs) = ponerOrdenado x (ordenar xs)

-- Propósito: dado un elemento 
-- y una lista ordenada de menor a mayor, 
-- devuelve una lista ordenada 
-- con ese elemento agregado
ponerOrdenado :: Int -> [Int] -> [Int]
ponerOrdenado n []     = [n]
ponerOrdenado n (x:xs) =
	if n > x
	   then x : ponerOrdenado n xs
	   else n : x : xs

--------------------------------------

-- Puedo agregar Eq
-- solo cuando son enumerativos

data Objeto = Chatarra | Tesoro deriving (Eq, Show)
data Camino = Fin 
            | Cofre [Objeto] Camino
            | Nada Camino deriving Show

hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn 0 Fin = False
hayTesoroEn n Fin = False
hayTesoroEn 0 (Cofre objs cam) =
	pertenece Tesoro objs
hayTesoroEn n (Cofre objs cam) =
	 hayTesoroEn (n - 1) cam
hayTesoroEn 0 (Nada cam) = False
hayTesoroEn n (Nada cam) =
	 hayTesoroEn (n - 1) cam

-----------------------------------

unoSiPerteneceTesoro objs =
	if pertenece Tesoro objs
	   then 1
	   else 0

cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre 0 m Fin = 0
cantTesorosEntre n m Fin = 0
cantTesorosEntre 0 m (Cofre objs cam) =
	  apariciones Tesoro objs 
	+ cantTesorosAntesDe m cam
cantTesorosEntre n m (Cofre objs cam) =
	cantTesorosEntre (n - 1) (m - 1) cam
cantTesorosEntre 0 m (Nada cam) =
	   cantTesorosAntesDe m cam
cantTesorosEntre n m (Nada cam) =
	cantTesorosEntre (n - 1) (m - 1) cam

cantTesorosAntesDe :: Int -> Camino -> Int
cantTesorosAntesDe 0 Fin = 0
cantTesorosAntesDe n Fin = 0
cantTesorosAntesDe 0 (Cofre objs cam) =
	apariciones Tesoro objs
cantTesorosAntesDe n (Cofre objs cam) =
	  apariciones Tesoro objs
	+ cantTesorosAntesDe (n - 1) cam
cantTesorosAntesDe 0 (Nada cam) = 0
cantTesorosAntesDe n (Nada cam) =
	 cantTesorosAntesDe (n - 1) cam

cantTesorosEntre' :: Int -> Int -> Camino -> Int
cantTesorosEntre' n m cam =
	cantTesorosAntesDe (m - n) (menosNCamino n cam)

menosNCamino :: Int -> Camino -> Camino
menosNCamino 0 Fin = Fin
menosNCamino n Fin = Fin
menosNCamino 0 (Cofre objs cam) =
	Cofre objs cam
menosNCamino n (Cofre objs cam) =
	menosNCamino (n - 1) cam
menosNCamino 0 (Nada cam) = 
	Nada cam
menosNCamino n (Nada cam) =
	menosNCamino (n - 1) cam

-- usando hasta
-- hasta 5 - hasta 3

------------------------------------

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

-- f EmptyT = ...
-- f (NodeT x ti td) =
-- 	...
-- 	f ti
-- 	f td

sumT EmptyT = 0
sumT (NodeT x ti td) =
	x + sumT ti + sumT td

sizeT EmptyT = 0
sizeT (NodeT x ti td) =
	1 +
	sizeT ti +
	sizeT td

mapDobleT EmptyT = EmptyT
mapDobleT (NodeT x ti td) =
	NodeT (x * 2)
	      (mapDobleT ti)
	      (mapDobleT td)

height EmptyT = 0
height (NodeT x ti td) =
	1 + max (height ti) (height td)

toList EmptyT = []
toList (NodeT x ti td) =
	x : (toList ti) ++ (toList td)

--------------------------------------

-- Los picantes

levelN :: Int -> Tree a -> [a]
levelN 0 EmptyT = []
levelN n EmptyT = []
levelN 0 (NodeT x ti td) =
	[x]
levelN n (NodeT x ti td) =
   (levelN (n - 1) ti)
   ++
   (levelN (n - 1) td)

-- levelN' :: Int -> Tree a -> [a]
-- levelN' n tree = levelNCon 0 n tree

-- levelNCon m n EmptyT = []
-- levelNCon m n (NodeT x ti td) =
-- 	if m == n
-- 	   then [x]
-- 	   else levelNCon (m + 1) n ti
-- 	        ++
-- 	        levelNCon (m + 1) n td


-- listPerLevel :: Tree a -> [[a]]
-- listPerLevel tree = hastaAltura (heightT t) tree

-- hastaAltura 0 EmptyT = []
-- hastaAltura n EmptyT = []
-- hastaAltura 0 (NodeT x ti td) =
-- 	[[x]]
-- hastaAltura n (NodeT x ti td) =
-- 	¿ x ?
-- 	hastaAltura (n - 1) ti
-- 	hastaAltura (n - 1) td

listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT x ti td) =
	[x] :
	concatPorPos (listPerLevel ti)
	             (listPerLevel td)

concatPorPos :: [[a]] -> [[a]] -> [[a]]
concatPorPos [] ys = ys
concatPorPos xs [] = xs
concatPorPos (x:xs) (y:ys) =
  (x ++ y) : (concatPorPos xs ys)

t = NodeT 1
		(NodeT 2
			(NodeT 3 EmptyT EmptyT) 
			(NodeT 4 EmptyT EmptyT))
		(NodeT 5
			(NodeT 6 EmptyT EmptyT) 
			(NodeT 7 EmptyT EmptyT))

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT x EmptyT EmptyT) = [[x]]
todosLosCaminos (NodeT x ti td) =
	agregarA x (todosLosCaminos ti)
	++
	agregarA x (todosLosCaminos td)

-- Propósito: agrega n a cada lista
agregarA :: a -> [[a]] -> [[a]]
agregarA n []  = []
agregarA n (x:xs) =
	(n : x) : agregarA n xs

------------------------------------

data Pizza = Prepizza
           | Capa Ingrediente Pizza
           deriving Show

data Ingrediente =
	  Salsa
	| Queso
	| Jamon
	| Anana
	| Aceitunas Int deriving Show

p1 = Capa Jamon (Capa Queso (Capa Salsa Prepizza))
p2 = Capa Anana (Capa Queso (Capa Salsa Prepizza))
p3 = Capa (Aceitunas 10) (Capa Queso (Capa Queso Prepizza))
p4 = Capa (Aceitunas 10) (Capa Queso (Capa (Aceitunas 20) Prepizza))

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa ing p) =
	1 + cantidadDeCapas p

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa ing p) =
	Capa (dupAcc ing) (duplicarAceitunas p)

-- esAceituna (Aceitunas n) = True
-- esAceituna _ = False

dupAcc (Aceitunas n) = Aceitunas (n * 2)
dupAcc ing = ing

-------------------------------------------

data Dir = Izq | Der

data Cofre = C [Objeto]

data Mapa = FinM Cofre
          | Bifurcacion Cofre Mapa Mapa

-------------------------------------------

type SectorId = String

type Tripulante = String

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril] deriving Show

data Barril = Comida | Oxigeno | Torpedo | Combustible deriving Show

data Sector = S SectorId [Componente] [Tripulante] deriving Show

-- registro
data Nave = N (Tree Sector) deriving Show

sectores :: Nave -> [SectorId]
sectores (N t) = sectoresT t

sectoresT :: Tree Sector -> [SectorId]
sectoresT EmptyT = []
sectoresT (NodeT x ti td) =
	idS x : sectoresT ti ++ sectoresT td

idS :: Sector -> SectorId
idS (S sid cps ts) = sid

-- no, porque mezcla varios niveles de abstracción
-- sectores :: Nave -> [SectorId]
-- sectores (N EmptyT) = []
-- sectores (N (NodeT (S sid cs ts) ti td)) = 
-- 	sid : sectores (N ti) ++ sectores (N td)

poderPropulsion :: Nave -> Int
poderPropulsion (N t) = poderPropulsionT t

poderPropulsionT :: Tree Sector -> Int
poderPropulsionT EmptyT = 0
poderPropulsionT (NodeT x ti td) =
	poderPropulsionS x
	+ poderPropulsionT ti
	+ poderPropulsionT td

poderPropulsionS :: Sector -> Int
poderPropulsionS (S sid cs ts) =
	poderPropulsionCMPS cs

poderPropulsionCMPS :: [Componente] -> Int
poderPropulsionCMPS [] = 0
poderPropulsionCMPS (c:cs) =
	poder c + poderPropulsionCMPS cs

	-- no hace falta preguntar
	-- if esMotor c
	--    then poder c + poderPropulsionCMPS cs
	--    else poderPropulsionCMPS cs

-- esMotor (Motor _) = True
-- esMotor _ = False

poder :: Componente -> Int
poder (Motor n) = n
poder _         = 0

------------------------------------------

type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo

data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo
          | Explorador Nombre [Territorio] Lobo Lobo
          | Cria Nombre

data Manada = M Lobo

buenaCaza (M l) = cantAlimento l > cantCrias l

cantAlimento :: Lobo -> Int
cantAlimento (Cazador n ps l1 l2 l3) = 
	length ps
	+ cantAlimento l1
	+ cantAlimento l2
	+ cantAlimento l3
cantAlimento (Explorador n ts l1 l2) = 
	cantAlimento l1
	+ cantAlimento l2
cantAlimento (Cria n) = 0

cantCrias :: Lobo -> Int
cantCrias (Cazador n ps l1 l2 l3) = 
	cantCrias l1
	+ cantCrias l2
	+ cantCrias l3
cantCrias (Explorador n ts l1 l2) = 
	cantCrias l1
	+ cantCrias l2
cantCrias (Cria n) = 1

-- buenaCazaL (Cazador n ps l1 l2 l3) = 
-- buenaCazaL (Explorador n ts l1 l2) = 
-- buenaCazaL (Cria n) = 

elAlfa :: Manada -> (Nombre, Int)
elAlfa (M l) = elAlfaL l

elAlfaL :: Lobo -> (Nombre, Int)
elAlfaL (Cazador n ps l1 l2 l3) = 
	maxP
	(maxP
	   (maxP (n, length ps) (elAlfaL l1))
	   (elAlfaL l2))
	(elAlfaL l3)
elAlfaL (Explorador n ts l1 l2) = 
	maxP (maxP (n, 0) (elAlfaL l1))
	     (elAlfaL l2)
elAlfaL (Cria n) = (n, 0)

maxP :: (Nombre, Int) -> (Nombre, Int) -> (Nombre, Int)
maxP (n1, x) (n2, y) =
	if x > y
	   then (n1, x)
	   else (n2, y)

-- observo este comportamiento
-- maximum :: [Int] -> Int
-- maximum [] = error "hiciste cualquiera"
-- maximum [x] = x
-- maximum (x:xs) = max x (maximum xs)

-- el maximo entre los 4
-- max (max (max w x) y) z

-- comparar con

-- operacion binaria
-- w + x + y + z

-- operaciones n-aria
-- sumar4 w x y z
-- sumar5 w x y z j
-- sumar6 w x y z j i

elAlfa' :: Manada -> (Nombre, Int)
elAlfa' (M l) = maxLS (conPresas l)

conPresas :: Lobo -> [(Nombre, Int)]
conPresas (Cazador n ps l1 l2 l3) = 
	(n, length ps) :
	conPresas l1
	++ conPresas l2
	++ conPresas l3
conPresas (Explorador n ts l1 l2) = 
	(n, 0) :
	conPresas l1
	++ conPresas l2
conPresas (Cria n) = [(n, 0)]

maxLS :: [(Nombre, Int)] -> (Nombre, Int)
maxLS []  = error "no va"
maxLS [x] = x
maxLS (x:xs) = 
	maxP x (maxLS xs)

--------------------------------------

exploradoresPorTerritorio :: 
   Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio (M l) = 
	expPorTL l

expPorTL :: Lobo -> [(Territorio, [Nombre])]
expPorTL (Cazador n ps l1 l2 l3) = 
	unirL (expPorTL l1)
	      (unirL (expPorTL l2)
	             (expPorTL l3))
expPorTL (Explorador n ts l1 l2) = 
	unirL 
	  (unirL (agregarTerritorios n ts) 
	  	     (expPorTL l1))
	  (expPorTL l2)
expPorTL (Cria n) = []

-- Propósito: arma una lista de tuplas donde
-- la primera componente es cada territorio
-- y la segunda componente es el nombre dado por parámetro
agregarTerritorios :: Nombre -> [Territorio] -> [(Territorio, [Nombre])]
agregarTerritorios = undefined

-- Propósito: une dos listas por territorio, 
-- donde para un mismo territorio se juntan las listas de nombres
unirL :: [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
unirL = undefined

