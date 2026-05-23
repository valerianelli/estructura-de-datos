sucesor :: Int -> Int
sucesor x = x + 1

sumar :: Int -> Int -> Int
sumar m n = m + n

divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto a b = ((div a b), (mod a b))

maxDelPar :: (Int,Int) -> Int
maxDelPar (n, m) = if n > m
					then n
					else m

-- sumar 2 (maxDelPar (divisionYResto 16 (sucesor 1)))   -- (2) (8, 0) (8) (10)

data Dir = Norte | Sur | Este | Oeste deriving Show 

opuesto :: Dir -> Dir
-- opuesto dir = if iguales dir Norte then Sur else ...
opuesto Norte = Sur
opuesto Sur   = Norte
opuesto Este  = Oeste
opuesto Oeste = Este


iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur     = True
iguales Este Este   = True
iguales Oeste Oeste = True
iguales _ _         = False

siguiente :: Dir -> Dir
-- precondicion: No puede pasar oeste, es parcial porque Oeste tira error.
siguiente Norte = Sur
siguiente Sur   = Este
siguiente Este  = Oeste
siguiente Oeste = error "no tiene siguiente"

------------------------------------------------
data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving Show

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes, Domingo)

empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes    = True
empiezaConM Miercoles = True
empiezaConM _         = False

--- subtarea

diaAInt :: DiaDeSemana -> Int
diaAInt Lunes     = 0
diaAInt Martes    = 1
diaAInt Miercoles = 2
diaAInt Jueves    = 3
diaAInt Viernes   = 4
diaAInt Sabado    = 5
diaAInt Domingo   = 6

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues d1 d2 = (diaAInt d1) > (diaAInt d2) 

estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes   = False
estaEnElMedio Domingo = False
estaEnElMedio _       = True

--Dado un booleano, si es True devuelve False, y si es False devuelve True.
--En Haskell ya está definida como not.
negar :: Bool -> Bool
negar True  = False
negar False = True

--Dados dos booleanos, si el primero es True y el segundo es False, 
--devuelve False, sino devuelve True.
implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _		   = True

--Dados dos booleanos, si ambos son True devuelve True, sino devuelve False.
--En Haskell ya está definida como &&.
and :: Bool -> Bool -> Bool
and True True = True
and _ _ 	  = False

--Dados dos booleanos, si alguno de ellos es True devuelve True, sino devuelve False.
--En Haskell ya está definida como ||.
or :: Bool -> Bool -> Bool
or False False = False
or _ _ 		   = True


--- TIPO REGISTRO
--Definir el tipo de dato Persona, como un nombre y la edad de la persona. 

-- data NombreDelTipoAlgebraicoBasico = ConstructorDelTipoBasico atributo1 atributo2 ...
-- Ejemplo



-- data MiBool = True | False

-- negar' :: MiBool -> MiBool
-- negar' True  = False
-- negar' False = True

-- negar' mb = ...

data Perro = PM String Int | PF String

esMasculino :: Perro -> Bool
esMasculino (PM _ _ ) = True
esMasculino _         = False

esFem :: Perro -> Bool
esFem (PF _) = True
esFem  _     = False

type NombreP = String
type EdadP = Int
data Persona = P NombreP EdadP

-- Por Pattern Matching
nombre :: Persona -> String
nombre (P n _) = n
 
edad :: Persona -> Int
edad (P _ e) = e

crecer :: Persona -> Persona
crecer (P n e) = P n (e + 1)


--Dados un nombre y una persona, devuelve una persona con la edad de la persona y el
--nuevo nombre.
cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nn (P n e) = P nn e  

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra (P _ e1) (P _ e2) = e1 > e2
-- esMayorQueLaOtra p1 p2 = edad p1 > edad p2

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = if (esMayorQueLaOtra p1 p2) 
						then p1
						else p2

data TipoDePokemon = Agua | Fuego | Planta
type Energia    = Int
type NombreE    = String
data Pokemon    = ConstP TipoDePokemon Energia
data Entrenador = ConstE NombreE Pokemon Pokemon

superior :: TipoDePokemon -> TipoDePokemon -> Bool
superior Agua Fuego   = True
superior Fuego Planta = True
superior Planta Agua  = True
superior _ _ 		  = False

superaA :: Pokemon -> Pokemon -> Bool
superaA (ConstP t1 _) (ConstP t2 _) = superior t1 t2

dameTipo :: Pokemon -> TipoDePokemon
dameTipo (ConstP t _) = t

cantidadDePokemonesDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonesDe tdp (ConstE _ p1 p2) = unoSisonMismoTipoDePokemon tdp (dameTipo p1)  +
	unoSisonMismoTipoDePokemon tdp (dameTipo p2) 

unoSisonMismoTipoDePokemon :: TipoDePokemon -> TipoDePokemon -> Int
unoSisonMismoTipoDePokemon Agua Agua = 1
unoSisonMismoTipoDePokemon Fuego Fuego = 1
unoSisonMismoTipoDePokemon Planta Planta = 1
unoSisonMismoTipoDePokemon _ _ = 0

--------
cantidadDePokemonesDe' :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonesDe' tdp (ConstE _ p1 p2) = auxCantidadDePokemonesDe tdp (dameTipo p1) (dameTipo p2)

auxCantidadDePokemonesDe :: TipoDePokemon -> TipoDePokemon -> TipoDePokemon -> Int
auxCantidadDePokemonesDe Agua t1 t2   = cantidadDeAgua t1 t2
auxCantidadDePokemonesDe Fuego t1 t2  = cantidadDeFuego t1 t2
auxCantidadDePokemonesDe Planta t1 t2 = cantidadDePlanta t1 t2

cantidadDeAgua :: TipoDePokemon -> TipoDePokemon -> Int
cantidadDeAgua Agua Agua = 2
cantidadDeAgua Agua _    = 1
cantidadDeAgua _ Agua    = 1
cantidadDeAgua _ _       = 0

cantidadDeFuego :: TipoDePokemon -> TipoDePokemon -> Int
cantidadDeFuego Fuego Fuego = 2
cantidadDeFuego Fuego _     = 1
cantidadDeFuego _ Fuego     = 1
cantidadDeFuego _ _         = 0

cantidadDePlanta :: TipoDePokemon -> TipoDePokemon -> Int
cantidadDePlanta Planta Planta = 2
cantidadDePlanta Planta _      = 1
cantidadDePlanta _ Planta      = 1
cantidadDePlanta _ _           = 0

----------------------------------------

extraerPokemones :: Entrenador -> [Pokemon]
extraerPokemones (ConstE _ p1 p2) = [p1, p2]

juntarPokemones :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemones (e1, e2) = (extraerPokemones e1) ++ (extraerPokemones e2)
-- juntarPokemones (ConstE _ p1 p2) (ConstE _ p3 p4) = [p1,p2,p3,p4]

-----------------------------------------------------------------------

--Dado un elemento de algún tipo devuelve ese mismo elemento.
loMismo :: a -> a
loMismo a = a

--Dado un elemento de algún tipo devuelve el número 7.
siempreSiete :: a -> Int
siempreSiete a = 7

--Dadas una tupla, invierte sus componentes.
--Por qué existen dos variables de tipo diferentes? Porque estan adentro de una tupla
swap :: (a, b) -> (b, a)
swap (a, b) = (b , a)
--Por qué estas funciones son polimórficas?
--Porque no definen un tipo especifico
-------------------------------------------------------------------------

--Defina las siguientes funciones polimórficas utilizando pattern matching sobre listas (no
--utilizar las funciones que ya vienen con Haskell)

--Dada una lista de elementos, si es vacía devuelve True, sino devuelve False.
--Definida en Haskell como null.

-- [] y (x:xs)  ---> [] o xs

estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _ = False

--Dada una lista devuelve su primer elemento.
--Definida en Haskell como head.
--Nota: tener en cuenta que el constructor de listas es :
elPrimero :: [a] -> a
elPrimero (x:xs) = x

--Dada una lista devuelve esa lista menos el primer elemento.
--Definida en Haskell como tail.
--Nota: tener en cuenta que el constructor de listas es :
sinElPrimero :: [a] -> [a]
sinElPrimero (x:xs) = xs

--Dada una lista devuelve un par, donde la primera componente es el primer elemento de la
--lista, y la segunda componente es esa lista pero sin el primero.
--Nota: tener en cuenta que el constructor de listas es :
splitHead :: [a] -> (a, [a])
splitHead (x:xs) = (x, xs)

-- [1, 2, 3] -> 1 : [2,3] -> 1 : 2 : [3] -> 1 : 2 : 3 : []
-- [1] -> 1: []

----------------------------------------------------

cantPokemonesSuperioresA :: TipoDePokemon -> Entrenador -> Int
-- Dado un entrenador decime cuantos pokemon le ganan a los de este tipo

superior' :: TipoDePokemon -> TipoDePokemon -> Int
superior' tp1 tp2    = if superior tp1 tp2 then 1 else 0

cantPokemonesSuperioresA tdp (ConstE _ p1 p2) = 
	superior' tdp (dameTipo p1) + superior' tdp (dameTipo p2)
-------------------------------------------------------------------------------------------------

dameEnergia :: Pokemon -> Energia
dameEnergia (ConstP _ e) = e

energiaSuperior :: Energia -> Energia -> Int
energiaSuperior e1 e2 = if (e1 > e2) 
						then 1
						else 0

conEnergiaSuperiorA :: Energia -> Entrenador -> Int
-- Dado un entrenador decime cuantos pokemon tienen mas energia que ...
conEnergiaSuperiorA e (ConstE _ p1 p2) = 
		(energiaSuperior (dameEnergia p1) e) + (energiaSuperior (dameEnergia p2) e)

-- Para pruebas

entrenadorcito :: Entrenador
poke1 = ConstP Agua 45
poke2 = ConstP Fuego 56
entrenadorcito = ConstE "Pepe" poke1 poke2

----------------------------- TIPO ALGEBRAICO VETERINARIA --------------------------------------
-- Defini un tipo de dato para una veterinaria.
-- Una veterinaria tiene un nombre y 4 veterinarios.
-- Los veterinarios tienen unos datos personales y una especialidad
-- Las especialidades pueden ser: Kinesiologia , Nutricion y Terapia.
-- Los datos personales son nombre apellido y edad


type NombreCentro = String
type NombreVet = String
type ApellVet = String
type EdadVet = Int
data Especialidad = Kinesiologia | Nutricion | Terapia deriving Show
data DatosPersonales = DP NombreVet ApellVet EdadVet deriving Show
data Vet = VT DatosPersonales Especialidad deriving Show
data CentroVeterinario = CV NombreCentro Vet Vet Vet Vet deriving Show

-- Implementar este tipo de datos como asi tambien los subtipos de datos que 
--encuentres necesarios.
--- Implementar las siguientes funciones:
---------------------------------------------------------------------------------
cantKinesio :: CentroVeterinario -> Int
-- Me da la cantidad de kinesiologos de la vete

dameEspecialidad :: Vet -> Especialidad
dameEspecialidad (VT _ e) = e

esKinesio :: Especialidad -> Int
eskinesio Kinesiologia = 1
esKinesio _    = 0

cantKinesio (CV _ v1 v2 v3 v4) = 
				(esKinesio (dameEspecialidad v1)) + 
				(esKinesio (dameEspecialidad v2)) + 
				(esKinesio (dameEspecialidad v3)) + 
				(esKinesio (dameEspecialidad v4))
---------------------------------------------------------------------------------
tieneNutriCuyaEdadEsMayorA :: Int -> CentroVeterinario -> Bool
-- Me dice si existen nutris mayor de X edad en la vete

esNutri :: Especialidad -> Bool
esNutri Nutricion    = True
esNutri _ = False

dameEdadDP :: DatosPersonales -> EdadVet
dameEdadDP (DP _ _ e) = e

dameEdadVet :: Vet -> EdadVet
dameEdadVet (VT dp _) = dameEdadDP dp

esNutriCuyaEdadEsMayorA :: Vet -> EdadVet -> Bool
esNutriCuyaEdadEsMayorA vt edad = 
	esNutri (dameEspecialidad vt) && dameEdadVet vt > edad


-- saco las partes y hago para el veterinario
tieneNutriCuyaEdadEsMayorA edad (CV _ v1 v2 v3 v4) = 
	 esNutriCuyaEdadEsMayorA v1 edad || esNutriCuyaEdadEsMayorA v2 edad || 
	 esNutriCuyaEdadEsMayorA v3 edad || esNutriCuyaEdadEsMayorA v4 edad

--------------------------------------
esTerapista :: Especialidad -> Bool
esTerapista Terapia    = True
esTerapista _ = False

dameApellidoDP :: DatosPersonales -> ApellVet
dameApellidoDP (DP _ a _) = a

dameApellidoVet :: Vet -> ApellVet
dameApellidoVet (VT dp _) = dameApellidoDP dp

esTerapistaConApellido :: Vet -> ApellVet -> Int
esTerapistaConApellido vt apellido = if esTerapista (dameEspecialidad vt) && 
	(dameApellidoVet vt == apellido) then 1 else 0

cantTerapistasConApellido :: String -> CentroVeterinario -> Int
-- Me dice la cantidad de terapistas con el apellido X en la vete
cantTerapistasConApellido apellido (CV _ v1 v2 v3 v4) = 
	 esTerapistaConApellido v1 apellido + esTerapistaConApellido v2 apellido + 
	 esTerapistaConApellido v3 apellido + esTerapistaConApellido v4 apellido 

{-
Tips para resolverlos:

1.  Define los tipos: Empieza definiendo los data para los tipos enumerados (ej:
    data Especialidad = ...), luego para los datos personales o componentes, y
    finalmente para el tipo principal.
2.  Pattern Matching: Como no hay listas, usa pattern matching en la definición
    de la función para "desarmar" el tipo principal:
    funcion (Mision d t1 t2 t3) = ... -- Donde t1, t2 y t3 son los tripulantes
3.  Funciones auxiliares: Crea funciones pequeñas que operen sobre el tipo
    interno (ej: sobre un solo Tripulante) y luego úsalas en la función
    principal para que el código sea más limpio.
-}

{-
1. El Equipo de Fórmula 1

Se busca modelar una escudería que compite en la F1.

  - Estructura: Una Escudería tiene un nombre y 2 Pilotos (el "Principal" y el
    "Reserva").
  - Pilotos: Tienen datos personales (nombre y edad) y un Rol (Novato, Veterano
    o Leyenda).
  - Funciones a implementar:
    1.  esEscuderiaJoven :: Escuderia -> Bool (Verdadero si ambos pilotos tienen
        menos de 25 años).
    2.  tieneLeyendaConNombre :: String -> Escuderia -> Bool (Verdadero si
        alguno de los dos pilotos es Leyenda y tiene el nombre indicado).
-}
type NombreEscu = String
type NombrePilo = String
type EdadPilo 	= Int
data Rol		= Novato | Veterano | Leyenda deriving Show
data Escuderia  = ES NombreEscu Piloto Piloto deriving Show
data Piloto     = PLT NombrePilo EdadPilo Rol deriving Show

esEscuderiaJoven :: Escuderia -> Bool
--Verdadero si ambos pilotos tienen menos de 25 años

dameEdadPiloto :: Piloto -> EdadPilo
dameEdadPiloto (PLT _ ep _) = ep

esMenorDe25 :: EdadPilo -> Bool
esMenorDe25 ep = 25 > ep 

esEscuderiaJoven (ES _ plt1 plt2) = 
					(esMenorDe25 (dameEdadPiloto plt1)) && 
					(esMenorDe25 (dameEdadPiloto plt2))

dameRol :: Piloto -> Rol
dameRol (PLT _ _ r) = r

dameNombrePilo :: Piloto -> NombrePilo
dameNombrePilo (PLT np _ _) = np

esLeyenda :: Rol -> Bool
esLeyenda Leyenda = True
esLeyenda _		  = False

tieneLeyendaConNombre :: String -> Escuderia -> Bool
--Verdadero si alguno de los dos pilotos es "Leyenda" y tiene el nombre indicado
tieneLeyendaConNombre nombre (ES _ plt1 plt2) =
    (nombre == dameNombrePilo plt1 && esLeyenda (dameRol plt1)) ||
    (nombre == dameNombrePilo plt2 && esLeyenda (dameRol plt2))

--------------------------------------------------------------------------------------------
{-

Modelar una misión a Marte.

  - Estructura: Una Misión tiene un destino y 3 Tripulantes.
  - Tripulantes: Tienen nombre, años de experiencia y una Función (Comandante,
    Ingeniero o Médico).
  - Funciones a implementar:
    1.  experienciaTotal :: Mision -> Int (Suma de los años de experiencia de
        los 3 tripulantes).
    2.  hayMedicoExperimentado :: Mision -> Bool (Verdadero si alguno de los
        tripulantes es Médico y tiene más de 10 años de experiencia).
-}
type NombreTripu = String
type Destino     = String
type Experiencia = Int
data Funcion 	 = Comandante | Ingeniero | Medico
data Tripulante  = TR NombreTripu Experiencia Funcion
data Mision 	 = MM Destino Tripulante Tripulante Tripulante

dameExp :: Tripulante -> Experiencia
dameExp (TR _ e _) = e

experienciaTotal :: Mision -> Int
experienciaTotal (MM _ t1 t2 t3) =
    dameExp t1 + dameExp t2 + dameExp t3

dameFunc :: Tripulante -> Funcion
dameFunc (TR _ _ f) = f

esMedico :: Funcion -> Bool
esMedico Medico = True
esMedico _		= False

esExperto :: Experiencia -> Bool
esExperto e = e > 10

hayMedicoExperimentado :: Mision -> Bool
hayMedicoExperimentado (MM _ t1 t2 t3) =
    (esMedico (dameFunc t1) && esExperto (dameExp t1)) ||
    (esMedico (dameFunc t2) && esExperto (dameExp t2)) ||
    (esMedico (dameFunc t3) && esExperto (dameExp t3))

--------------------------------------------------------------------------------------------
{-
3. Estudio de Grabación

Modelar una sesión de grabación musical.

  - Estructura: Una Sesión tiene un género y 2 Músicos (Solista y Acompañante).
  - Músicos: Tienen nombre, instrumento (Guitarra, Piano o Batería) y nivel de
    habilidad (un número de 1 a 10).
  - Funciones a implementar:
    1.  promedioHabilidad :: Sesion -> Float (El promedio de habilidad de ambos
        músicos).
    2.  suenaBien :: Sesion -> Bool (Verdadero si el instrumento del solista es
        Piano y su habilidad es mayor a 8).
-}

type Habilidad 	 = Int
type Genero      = String
type NomMusico   = String
data Instrumento = Guitarra | Piano | Bateria
data Musico 	 = MU NomMusico Instrumento Habilidad
type Solista 	 = Musico
type Acompañante = Musico
data Sesion 	 = SS Genero Solista Acompañante





--------------------------------------------------------------------------------------------
{-
4. Bufete de Abogados

Modelar un estudio jurídico.

  - Estructura: Un Estudio tiene una dirección y 3 Socios.
  - Socios: Tienen nombre, apellido y una Especialidad (Penal, Civil o Laboral).
  - Funciones a implementar:
    1.  mismoApellido :: String -> Estudio -> Bool (Verdadero si los 3 socios
        tienen el mismo apellido que se pasa por parámetro).
    2.  cantidadEspecialistas :: Especialidad -> Estudio -> Int (Devuelve
        cuántos de los 3 socios pertenecen a la especialidad indicada).
-}

{-
5. Configuración de PC Gamer

Modelar la arquitectura de una computadora.

  - Estructura: Una PC tiene una Marca y 3 Componentes principales (Procesador,
    Placa de Video y Fuente).
  - Componentes: Tienen modelo, consumo en Watts y un Estado (Nuevo, Usado o
    Reacondicionado).
  - Funciones a implementar:
    1.  consumoTotal :: PC -> Int (Suma de los Watts de los 3 componentes).
    2.  esPCConfiable :: PC -> Bool (Verdadero si todos sus componentes son
        "Nuevo").
-}

{-
6. Sistema de Seguridad para el Hogar

Modelar una alarma inteligente.

  - Estructura: Un Sistema tiene un código de activación y 3 Sensores
    instalados.
  - Sensores: Tienen una ubicación (ej: "Puerta"), sensibilidad (1 a 5) y Tipo
    (Movimiento, Calor o Apertura).
  - Funciones a implementar:
    1.  haySensorPeligroso :: Sistema -> Bool (Verdadero si algún sensor de
        "Calor" tiene sensibilidad 5).
    2.  sensorEnUbicacion :: String -> Sistema -> Bool (Verdadero si alguno de
        los 3 sensores está en la ubicación indicada).
-}

{-
7. Taller de Reparación de Celulares

Modelar una orden de reparación.

  - Estructura: Una Orden tiene un número de ID y 2 Técnicos asignados.
  - Técnicos: Tienen nombre, sueldo base y Categoría (Junior, SemiSenior o
    Senior).
  - Funciones a implementar:
    1.  presupuestoManoDeObra :: Orden -> Float (Suma de los sueldos de ambos
        técnicos si un técnico es Senior, su sueldo cuenta por 1.5).
    2.  tieneTecnicoJunior :: Orden -> Bool (Verdadero si alguno de los dos
        técnicos es Junior).
-}

{-
8. Restaurante de Sushi

Modelar un Combo de Sushi.

  - Estructura: Un Combo tiene un nombre y 4 Piezas de sushi.
  - Piezas: Tienen un ingrediente principal, cantidad de calorías y Tipo (Roll,
    Nigiri o Sashimi).
  - Funciones a implementar:
    1.  esComboLight :: Combo -> Bool (Verdadero si el total de calorías de
        las 4 piezas es menor a 500).
    2.  contarNigirisDeSalmon :: String -> Combo -> Int (Cuenta cuántas piezas
        son tipo "Nigiri" y tienen como ingrediente "Salmón").
-}

{-
9. Laboratorio de Química

Modelar un estante de reactivos químicos.

  - Estructura: Un Estante tiene una sección (A, B o C) y 3 Frascos.
  - Frascos: Tienen nombre del compuesto, nivel de toxicidad (1 a 10) y Estado
    (Líquido, Sólido o Gaseoso).
  - Funciones a implementar:
    1.  peligroExplosion :: Estante -> Bool (Verdadero si al menos 2 frascos son
        "Gaseoso" y su toxicidad es mayor a 7).
    2.  compuestoPresente :: String -> Estante -> Bool (Verdadero si el nombre
        del compuesto buscado coincide con alguno de los 3 frascos).
-}

{-
10. Agencia de Viajes (Paquetes)

Modelar un paquete turístico personalizado.

  - Estructura: Un Paquete tiene un destino y 3 Actividades incluidas.
  - Actividades: Tienen descripción, costo y Clasificación (Aventura, Cultural o
    Relax).
  - Funciones a implementar:
    1.  esPaqueteAventurero :: Paquete -> Bool (Verdadero si todas las
        actividades son de clasificación "Aventura").
    2.  costoActividadesCulturals :: Paquete -> Float (Suma del costo de
        aquellas actividades que sean de clasificación "Cultural").
-}








