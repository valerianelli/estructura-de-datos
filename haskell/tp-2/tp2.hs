-- Dada una lista de enteros devuelve la suma de todos sus elementos.
sumatoria :: [Int] -> Int
sumatoria [] 	 = 0
sumatoria (x:xs) = x + (sumatoria xs)

sumatoriaNoAceptaVacia :: [Int] -> Int
-- separas caso de error con el resto del computo
sumatoriaNoAceptaVacia [] = error ""
sumatoriaNoAceptaVacia l  = sumatoria l

-------------------------------------------------------------------------------
-- Dada una lista de elementos de algún tipo devuelve el largo de esa lista, 
-- es decir, la cantidad de elementos que posee.
longitud :: [a] -> Int
longitud [] 	= 0
longitud (x:xs) = 1 + (longitud xs)

--------------------------------------------------------------------------------
-- Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
sucesores :: [Int] -> [Int]
sucesores [] 	 = []
sucesores (x:xs) = (x + 1) : sucesores xs

---------------------------------------------------------------------------------
-- Dada una lista de booleanos devuelve True si todos sus elementos son True.
conjuncion :: [Bool] -> Bool    
conjuncion [] 	  = True
conjuncion (x:xs) = x && (conjuncion xs)

--------------------------------------------------------------------------------
-- Dada una lista de booleanos devuelve True si alguno de sus elementos es True.
disyuncion :: [Bool] -> Bool
disyuncion [] 		 = False
-- disyuncion (x:xs) = x || (disyuncion xs)
disyuncion (x:xs) 	 = if x == True 
					   then True
					   else disyuncion xs
						
{-
def disyuncion(xs):
	if xs == []:
		return False
	else:
		if x == True:
			return True
		else:
			return disyuncion xs
-}

---------------------------------------------------------------------------------
-- Dada una lista de listas, devuelve una única lista con todos sus elementos.
aplanar :: [[a]] -> [a]
aplanar [] 	      = []
-- aplanar (x:[]) = [x] ESTE CASO ES INNESESARIO
aplanar (x:xs)    = x ++ (aplanar xs)

----------------------------------------------------------------------------------
-- Dados un elemento e y una lista xs devuelve True si existe un elemento en 
-- xs que sea igual a e.
pertenece :: Eq a => a -> [a] -> Bool
pertenece e [] 	   = False
pertenece e (x:xs) = if e == x 
					 then True 
					 else pertenece e xs
-- pertenece e (x:xs) = e == x || pertenece e xs

-- f :: [a] -> b
-- f []     = -- identidad de B
-- f (x:xs) =  .... x  ....   f xs

-- def sonIguales(a1, a2):
--	 return a1 == a2

--------------------------------------------------------------------------------
-- Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
apariciones :: Eq a => a -> [a] -> Int
apariciones e [] 	 = 0
apariciones e (x:xs) = if e == x
					   then 1 + (apariciones e xs)
					   else apariciones e xs

-- REHACER apariciones'
apariciones' :: Eq a => a -> [a] -> Int
apariciones' e [] 	 = 0
apariciones' e (x:xs) = unoSiSonIguales (e == x) + apariciones e xs

unoSiSonIguales :: Bool -> Int
unoSiSonIguales True = 1
unoSiSonIguales False = 0

-----------------------------------------------------------------------------------
-- Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA e []     = []
losMenoresA e (x:xs) = if e > x -- primero que separo condicionBooleano
                       then x : (losMenoresA e xs) -- que agrego en este caso "x"
                       else losMenoresA e xs -- donde agrego aca: "losMenoresA e xs"
                       -- que se cumple en los 2 casos (para True y False)
                       -- el tipo de retorno en la subfuncion tiene que coincidir con el 
                       -- de la principal (o ser facilmente convertido a eso)

-- REHACER losMenoresA'
losMenoresA' :: Int -> [Int] -> [Int]
losMenoresA' e []     = []
losMenoresA' e (x:xs) = agregarSi (e > x) x (losMenoresA' e xs)


agregarSi :: Bool -> Int -> [Int] -> [Int]
agregarSi True x xs = x:xs
agregarSi False _ xs = xs

---------------------------------------------------------------------------------
-- Dados un número n y una lista de listas, devuelve la lista de aquellas listas 
-- que tienen más de n elementos.
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n [] 	   = []
lasDeLongitudMayorA n (xs:xss) = if n < (longitud xs)
                                 then xs : (lasDeLongitudMayorA n xss)
                                 else lasDeLongitudMayorA n xss

agregarSi' :: Bool -> [a] -> [[a]] -> [[a]]
agregarSi' True  xs xss = xs:xss
agregarSi' False _  xss = xss

-- REHACER lasDeLongitudMayorA'
lasDeLongitudMayorA' :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA' n [] 	   = []
lasDeLongitudMayorA' n (xs:xss) = agregarSi' (n < (longitud xs)) xs (lasDeLongitudMayorA' n xss)

----------------------------------------------------------------------------------
-- Dados una lista y un elemento, devuelve una lista con ese elemento agregado 
-- al final de la lista.

-- PATRON: " Llegar al caso base y unir al final "
-- (agregarAlFinal xs e) :: [a] y denota de la lista original (x:xs) se queda con su
-- tail (o sea xs) y le concatena e. 
-- Es decir, la lista sin el primero pero con e al final.
-- Fijate una ejecucion (no es lo mejor):
-- agregarAlFinal (1: [2,3]) 4 --- por ec2  --> 
--		1 : agregarAlFinal (2: [3]) 4 ------ por ec2------>
--          2 : agregarAlFinal (3: []) 4 -----por ec2----->
--              3 : agregarAlFinal ([]) 4 ------por ec1-----> (
-- 					[4]
--  Lo cual es 1 : 2 : 3 : [4] -> [1,2,3,4]

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] e 	    = [e] -- ec1
agregarAlFinal (x:xs) e 	= x : (agregarAlFinal xs e) --ec2

-----------------------------------------------------------------------
-- Dadas dos listas devuelve la lista con todos los elementos de la primera lista 
-- y todos los elementos de la segunda a continuación. Definida en Haskell como ++.

-- PATRON: Saco en el 2do y pongo en el 1ro. Fuerzo a llegar a un caso base
-- de la ecuacion 2
concatenar :: [a] -> [a] -> [a]
concatenar [] xs     = xs 
concatenar xs []     = xs -- ecuacion 2
concatenar xs (y:ys) = concatenar (agregarAlFinal xs y) ys -- fijese que saco en la 2da aca
					-- concatenar poneEnPrimera sacaEnSegunda	

-----------------------------------------------------------------------------------
-- Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Definida
-- en Haskell como reverse.
reversa' :: [a] -> [a]
reversa' [] 	= []
reversa' (x:xs) = concatenar (reversa' xs) [x]
-- reversa' (1:[2,3,4]) = concatenar (reversa' [2,3,4] ) [1] -- confio--> concatenar ([4,3,2]) [1]


reversa :: [a] -> [a]
reversa xs = reversaAux [] xs

reversaAux :: [a] -> [a] -> [a]
reversaAux xs []     = xs
reversaAux xs (y:ys) = reversaAux (y:xs) (ys) 

-- [1,2,3,4] 	---> ys x:xs --- [] [1,2,3,4]
			-- -> [] [1,2,3,4]  ---- (x : ys ) xs   
			-- -> [1] [2,3,4]    
			-- -> [2,1] [3, 4] 
			-- -> [3,2,1] [4] 
			-- -> [4,3,2,1] [] 
			-- -> ys __
-- Analisis
-- concatenar (agregarAlFinal xs y) ys
-- reversaAux (y:xs) (ys)

-- se puede reescribir
-- f (agregarAlFinal xs y) ys
-- f' (y : xs) ys

-- La diferencia es que concatenar agrega el final y reversaAux al ppio.
-- Entonces lo que hicimos fue:
-- Resuelvo reversa. Encontre una solucion algoritmica y esa solucion me llevo a reversaAux.
-- ReversaAux es casi igual a concatenar. 
-- El patron es saco de una pongo en otra bajo un criterio.

-------------------------------------------------------------------------------------
-- Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el
-- máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
-- las listas no necesariamente tienen la misma longitud.
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos xs [] 		 = []
zipMaximos [] ys 		 = []
zipMaximos (x:xs) (y:ys) =  if x > y
							then x : zipMaximos xs ys
							else y : zipMaximos xs ys

-- REHACER concatenar'

---------------------------------------------------------------------------------
-- Dada una lista devuelve el mínimo
elMinimo :: Ord a => [a] -> a
elMinimo [] 	= error "La lista debe tener como mínimo un elemento"
elMinimo [x] 	= x
elMinimo (x:xs) = if x < (elMinimo xs)
				  then x
				  else elMinimo xs

---------------------------------------------------------------------------------
-- Dado un número n se devuelve la multiplicación de este número y todos sus anteriores hasta
-- llegar a 0. Si n es 0 devuelve 1. La función es parcial si n es negativo.
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)



------------------------------------------------------------------------------------
-- Dado un número n devuelve una lista cuyos elementos sean los números comprendidos
-- entre n y 1 (incluidos). Si el número es inferior a 1, devuelve la lista vacía.
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva n = n : cuentaRegresiva (n - 1)

---------------------------------------------------------------------------------------
-- Dado un número n y un elemento e devuelve una lista en la que el elemento e se repite n veces.
repetir :: Int -> a -> [a]
repetir 0 e = []
repetir n e = e : repetir (n - 1) e

---------------------------------------------------------------------------------------
-- Dados un número n y una lista xs, devuelve una lista con los n primeros elementos de xs.
-- Si la lista es vacía, devuelve una lista vacía.
losPrimeros :: Int -> [a] -> [a]
losPrimeros n [] = []
losPrimeros n xs = losPrimeros' n xs

losPrimeros' :: Int -> [a] -> [a]
losPrimeros' 0 xs 	   = []
losPrimeros' n (x:xs)  = x : losPrimeros' (n - 1 ) xs

--------------------------------------------------------------------------------------
-- Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista
-- recibida. Si n es cero, devuelve la lista completa.
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros n [] = []
sinLosPrimeros n xs = sinLosPrimeros' n xs

sinLosPrimeros' :: Int -> [a] -> [a]
sinLosPrimeros' 0 xs      = xs
sinLosPrimeros' n (x: xs) = sinLosPrimeros' (n - 1) xs


--------------------------------------------------------------------------------------
type NombreP = String
type EdadP   = Int
data Persona = P NombreP EdadP

edad :: Persona -> Int
edad (P _ e) = e

-----------------------------------------------------------------------------------
-- Definir el tipo de dato Persona, como un nombre y la edad de la persona. 
-- Realizar las siguientes funciones:
-- Dados una edad y una lista de personas devuelve todas las personas que son mayores
-- a esa edad.
mayoresA :: Int -> [Persona] -> [Persona]
mayoresA 0	ps	  = ps
mayoresA n (p:ps) = if n < (edad p) 
					then p : mayoresA n ps
					else mayoresA n ps

agregarCuando :: Bool -> Persona -> [Persona] -> [Persona]
agregarCuando True p ps  = p:ps
agregarCuando False _ ps = ps
				
-- REHACER mayoresA'	
mayoresA' :: Int -> [Persona] -> [Persona]
mayoresA' 0	ps	   = ps
mayoresA' n (p:ps) = agregarCuando (n < (edad p)) p (mayoresA' n ps)

------------------------------------------------------------------------------------
-- Dada una lista de personas devuelve el promedio de edad entre esas personas.
-- Precon-dición: la lista al menos posee una persona.
promedioEdad :: [Persona] -> Int
promedioEdad ps = div (sumarEdades ps) (longitud ps)
		  	
sumarEdades :: [Persona] -> Int
sumarEdades [] = 0
sumarEdades (p:ps) = edad p + sumarEdades ps

------------------------------------------------------------------------------
-- Dada una lista de personas devuelve la persona más vieja de la lista. Precondición: la
-- lista al menos posee una persona.
-- type NombreP = String
-- type EdadP = Int
-- data Persona = P NombreP EdadP

dameElMasViejo :: Persona -> Persona -> Persona
dameElMasViejo p1 p2 = if edad p1 > edad p2 then p1 else p2

elMasViejo :: [Persona] -> Persona
elMasViejo [p] = p
elMasViejo (p:ps) = dameElMasViejo p (elMasViejo ps)

--------------------------------------------
data TipoDePokemon = Agua | Fuego | Planta
data Pokemon = ConsPokemon TipoDePokemon Int
data Entrenador = ConsEn String [Pokemon]

-- Devuelve la cantidad de pokémon que posee el entrenador.
cantPokemones :: Entrenador -> Int
cantPokemones (ConsEn _ lp) = longitud lp

--------------------------------------------------------------------
-- Devuelve la cantidad de pokémon de determinado tipo que posee el entrenador.
{-
damePokemonesDe :: Entrenador -> [Pokemon]
damePokemones (ConsEn _ lp) = lp

dameLosTiposDePk :: [Pokemon] -> [TipoDePokemon]
dameLosTiposDePk [] = []
dameLosTiposDePk (p:ps) = (p dameSuTipo) : dameLosTiposDePk ps

sonMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
sonMismoTipo tp1 tp2 = tp1 == tp2

contameMismoTipo :: TipoDePokemon -> [TipoDePokemon] -> Int
contameMismoTipo t1 [] = 0
contameMismoTipo t1 (t:ts) = 
	let anteriores = contameMismoTipo ts
	in if sonMismoTipo t1 t then 1 + anteriores else anteriores
					   					   
cantPokemonesDe :: TipoDePokemon -> Entrenador -> Int     
cantPokemonesDe tipo e =  contameMismoTipo tipo (dameLosTiposDePk (damePoemonesDe e))
-}

sonMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
sonMismoTipo Fuego Fuego = True
sonMismoTipo Planta Planta = True
sonMismoTipo Agua Agua = True
sonMismoTipo _ _ = False

dameTipo :: Pokemon -> TipoDePokemon
dameTipo (ConsPokemon t _ ) = t

cantPokemonesDe :: TipoDePokemon -> Entrenador -> Int     
cantPokemonesDe tipo (ConsEn _ lp) = contameMismo tipo lp  






contameMismo :: TipoDePokemon -> [Pokemon] -> Int
contameMismo t []     = 0
contameMismo t (x:xs) = 
	   let anteriores = contameMismo t xs in
	   if sonMismoTipo t (dameTipo x)  then 1 + anteriores else anteriores

--unoSiSonIguales :: Bool -> Int
--unoSiSonIguales True = 1
--unoSiSonIguales False = 0

-- REHACER contameMismo'
contameMismo' :: TipoDePokemon -> [Pokemon] -> Int
contameMismo' t []     = 0
contameMismo' t (x:xs) = unoSiSonIguales (sonMismoTipo t (dameTipo x)) + (contameMismo' t xs)

------------------
contameMismo'' :: TipoDePokemon -> [Pokemon] -> Int
contameMismo'' t []     = 0
contameMismo'' t (x:xs) = sumaUnoSiSonIguales (sonMismoTipo t (dameTipo x)) (contameMismo'' t xs)

sumaUnoSiSonIguales :: Bool -> Int -> Int
sumaUnoSiSonIguales True x  = 1 + x
sumaUnoSiSonIguales False x = x

--------------------------------------------------------------------------------------------
-- Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo,
-- que le ganarían a los Pokemon del segundo entrenador.

tieneFuego :: Entrenador -> Bool
tieneFuego e = (cantPokemonesDe Fuego e) > 0 

tieneAgua :: Entrenador -> Bool
tieneAgua e = (cantPokemonesDe Agua e) > 0 

tienePlanta :: Entrenador -> Bool
tienePlanta e = (cantPokemonesDe Planta e) > 0 

cuantasPlantas :: Entrenador -> Int
cuantasPlantas e = cantPokemonesDe Planta e

cuantosFuegos :: Entrenador -> Int
cuantosFuegos e = cantPokemonesDe Fuego e

cuantasAguas :: Entrenador -> Int
cuantasAguas e = cantPokemonesDe Agua e

losQueLeGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int
losQueLeGanan Fuego  e1 e2 = if tieneFuego e1 then cuantasPlantas e2 else 0
losQueLeGanan Agua   e1 e2 = if tieneAgua e1 then cuantosFuegos e2 else 0
losQueLeGanan Planta e1 e2 = if tienePlanta e1 then cuantasAguas e2 else 0

----------------------------------------------------------------------------
-- Dado un entrenador, devuelve True si posee al menos un pokémon de cada tipo posible.
esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon e = tieneFuego e && tieneAgua e && tienePlanta e

--------------------------------------------------------------------------------------------
data Seniority = Junior | SemiSenior | Senior
data Proyecto = ConsProyecto String
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
data Empresa = ConsEmpresa [Rol]

-- Dada una empresa denota la lista de proyectos en los que trabaja, 
-- sin elementos repetidos.

dameProyecto :: Rol -> Proyecto
dameProyecto (Developer  _ p) = p
dameProyecto (Management _ p) = p 

dameProyectos :: [Rol] -> [Proyecto]
dameProyectos []     = []
dameProyectos (x:xs) = (dameProyecto x) : dameProyectos xs

proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa xs) = dameProyectos xs

---------------------------------------------------------------------------------
-- Dada una empresa indica la cantidad de desarrolladores senior que posee.

esDevSenior :: Rol -> Bool
esDevSenior (Developer Senior _) = True
esDevSenior _                    = False

dameDevSeniors :: [Rol] -> [Rol]
dameDevSeniors [] = []
dameDevSeniors (x:xs) = let restoDevSeniors = dameDevSeniors xs in
	if (esDevSenior x) then x : restoDevSeniors else restoDevSeniors

agregarSi'' :: Bool -> Rol -> [Rol] -> [Rol]
agregarSi'' True x xs = x:xs
agregarSi'' False _ xs = xs
	
dameDevSeniors' :: [Rol] -> [Rol]
dameDevSeniors' [] = []
dameDevSeniors' (x:xs) = agregarSi'' (esDevSenior x) x (dameDevSeniors' xs )

losDevSenior :: Empresa -> Int
losDevSenior (ConsEmpresa xs) = longitud (dameDevSeniors xs)

------------------------------------------------------------------------------
-- Indica la cantidad de empleados que trabajan en alguno de los proyectos dados
-- cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int

