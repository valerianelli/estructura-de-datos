----------------------------------------------------------------------------------
-- Representaremos una celda con bolitas de colores rojas y azules, 
-- de la siguiente manera:
data Color = Azul | Rojo deriving Show
data Celda = Bolita Color Celda | CeldaVacia deriving Show

-- En dicha representación, la cantidad de apariciones de un determinado color 
-- denota la cantidad de bolitas de ese color en la celda.
-- Por ejemplo, una celda con 2 bolitas azules y 2 rojas, 
-- podría ser la siguiente:
-- Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))
-- Implementar las siguientes funciones sobre celdas:

-- Dados un color y una celda, indica la cantidad de bolitas de ese color. 

esMismoColor :: Color -> Color -> Bool
esMismoColor Azul Azul = True
esMismoColor Rojo Rojo = True
esMismoColor _ _ = False

nroBolitas :: Color -> Celda -> Int
nroBolitas cr CeldaVacia = 0
nroBolitas cr1 (Bolita cr2 celda) = if (esMismoColor cr1 cr2) 
									 then  1 + (nroBolitas cr1 celda)
									 else nroBolitas cr1 celda

sumaUnoSi :: Bool -> Int -> Int
sumaUnoSi True i  = 1 + i
sumaUnoSi False i = i
									 
nroBolitas' :: Color -> Celda -> Int
nroBolitas' cr CeldaVacia = 0
nroBolitas' cr1 (Bolita cr2 celda) = sumaUnoSi (esMismoColor cr1 cr2) (nroBolitas' cr1 celda)

---------------------------------------------------------------------------------------------
-- Dado un color y una celda, agrega una bolita de dicho color a la celda.
-- data Color = Azul | Rojo
-- data Celda = Bolita Color Celda | CeldaVacia
poner :: Color -> Celda -> Celda				
poner cr celda = Bolita cr (celda)			
										
---------------------------------------------------------------------------------------------
-- Dado un color y una celda, quita una bolita de dicho color de la celda. 
-- Nota: esta función es total.								
sacar :: Color -> Celda -> Celda
sacar cr CeldaVacia = CeldaVacia
sacar cr (Bolita cr2 celda) =
    if esMismoColor cr cr2
        then celda
        else poner cr2 (sacar cr celda)
----------------------------------------------------------------------------------------------
-- Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda.
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 cr celda = celda
ponerN n cr celda = poner cr (ponerN (n - 1) cr celda)

----------------------------------------------------------------------------------------------
-- Tenemos los siguientes tipos de datos 
data Objeto = Cacharro | Tesoro
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
-- Definir las siguientes funciones:

-- Indica si hay un cofre con un tesoro en el camino.
hayTesoro :: Camino -> Bool


-- Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro.
-- Si un cofre con un tesoro está al principio del camino, la cantidad de pasos a recorrer es 0.
-- Precondición: tiene que haber al menos un tesoro.
-- pasosHastaTesoro :: Camino -> Int


-- Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de
-- pasos es 5, indica si hay un tesoro en 5 pasos.
-- hayTesoroEn :: Int -> Camino -> Bool


-- Indica si hay al menos “n” tesoros en el camino.
-- alMenosNTesoros :: Int -> Camino -> Bool


-- Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango.
-- Por ejemplo, si el rango es 3 y 5, indica la cantidad de tesoros que hay
-- entre hacer 3 pasos y hacer 5. Están incluidos tanto 3 como 5 en el resultado.
-- cantTesorosEntre :: Int -> Int -> Camino -> Int


















