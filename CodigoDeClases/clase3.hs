-- Tipos algebraicos
  -- Tipos enumerativos (Dir, Color, Bool)
  -- Registros (Persona)
  -- Tipos recursivos (Listas)
-- Recursión estructural
-- Pattern Matching
-- Polimorfismo paramétrico
-- Funciones y tipos

data Dir = Norte | Sur
data Persona = ConsP String Int

data Fila = Nadie
          | Primero Int String Fila deriving Show

perteneceDNI :: Int -> Fila -> Bool
perteneceDNI d Nadie = False
perteneceDNI d (Primero dni trm fs) =
    d == dni || perteneceDNI d fs

tramites :: Fila -> [String]
tramites Nadie = []
tramites (Primero dni trm fs) =
    trm : tramites fs

unirFilas :: Fila -> Fila -> Fila
unirFilas Nadie fs2 = fs2
unirFilas (Primero dni trm fs) fs2 =
    Primero dni trm (unirFilas fs fs2)

elMasViejo :: Fila -> Int
elMasViejo Nadie = error "no hay dnis"
elMasViejo (Primero dni trm Nadie) = dni
elMasViejo (Primero dni trm fs) =
    min dni (elMasViejo fs)

conEstosDNI :: [Int] -> String -> Fila 
conEstosDNI [] trm = Nadie
conEstosDNI (x:xs) trm =
    Primero x trm (conEstosDNI xs trm)

-- hayDNIRepetido :: Fila -> Bool

estaVacia :: Fila -> Bool
estaVacia Nadie = True
estaVacia f = False

fila1 :: Fila
fila1 = Nadie

fila2 :: Fila
fila2 = Primero 12903 "Renuncia" Nadie

fila3 :: Fila
fila3 = Primero 12390 "Nacimiento"
          (Primero 12323 "Defuncion" Nadie)

fila4 :: Fila
fila4 = unirFilas fila2 fila3

-- Primero 12903 "Renuncia" Nadie
      -- (Primero 12390 "Nacimiento"
      --     (Primero 12323 "Defuncion" Nadie))

longitudFila :: Fila -> Int
longitudFila Nadie = 0
longitudFila (Primero dni trm fs) =
    1 + longitudFila fs

-- longitudFila Primero 12390 "Nacimiento"
--               (Primero 12323 "Defuncion" Nadie)
-- -> def longitudFila
-- 1 + longitudFila (Primero 12323 "Defuncion" Nadie)
-- -> def longitudFila
-- 1 + (1 + longitudFila Nadie)
-- -> def longitudFila
-- 1 + (1 + 0)
-- ->
-- 2

-- data [a] = [] | a : [a]

-- longitud [] = ...
-- longitud (x:xs) = ... longitud xs

---------------------------------------

data Tostada = Mermelada String Tostada
             | Pan
             deriving Show

cuantasMermeladas :: Tostada -> Int
cuantasMermeladas Pan = 0
cuantasMermeladas (Mermelada m ts)  =
    1 + cuantasMermeladas ts

tostada1 :: Tostada
tostada1 = Mermelada "membrillo"
             (Mermelada "batata" Pan)

sinNada :: Tostada -> Bool
sinNada Pan = True
sinNada _   = False

alPiso :: Tostada -> Tostada
alPiso Pan = Pan
alPiso (Mermelada m t) =
    agregarAlFinalT m (alPiso t)

agregarAlFinalT :: String -> Tostada -> Tostada
agregarAlFinalT nm Pan = Mermelada nm Pan
agregarAlFinalT nm (Mermelada m t) =
    Mermelada m (agregarAlFinalT nm t)

----------------------------------------

data Tren = Vagon Int Tren
          | Fin deriving Show

cantidadPasajeros :: Tren -> Int
cantidadPasajeros Fin = 0
cantidadPasajeros (Vagon n t) =
    n + cantidadPasajeros t

duplicarPasajeros :: Tren -> Tren
duplicarPasajeros Fin = Fin
duplicarPasajeros (Vagon n t) =
    Vagon (n*2) (duplicarPasajeros t)

unirTrenes :: Tren -> Tren -> Tren
unirTrenes Fin t2 = t2
unirTrenes (Vagon n t) t2 =
    Vagon n (unirTrenes t t2)

tren1 = Vagon 21 (Vagon 30 (Vagon 3 Fin))

-------------------------------------

data Objeto = Moneda | Arma | Escudo deriving Show

data Camino = Lugar Objeto Camino
            | UltimoLugar Objeto deriving Show

camino1 :: Camino
camino1 =
    Lugar Moneda 
       (Lugar Arma 
        (Lugar Escudo 
            (Lugar Moneda 
                (UltimoLugar Arma))))

objeto :: Camino -> Objeto
objeto (UltimoLugar o) = o
objeto (Lugar o cam)   = o

objetos :: Camino -> [Objeto]
objetos (UltimoLugar o) = [o]
objetos (Lugar o cam)   =
    o : objetos cam

monedas :: Camino -> [Objeto]
monedas (UltimoLugar o) =
    if esMoneda o
        then [o]
        else []
monedas (Lugar o cam)   =
    if esMoneda o
        then o : monedas cam
        else monedas cam

esMoneda :: Objeto -> Bool
esMoneda Moneda = True
esMoneda _      = False

-------------------------------------

data Mapa = FinM
          | Bifurcacion Objeto Mapa Mapa deriving Show

-- f FinM = ...
-- f (Bifurcacion o m1 m2) = 
--     ... f m1 ... f m2 ...

sizeM :: Mapa -> Int
sizeM FinM = 0
sizeM (Bifurcacion o m1 m2) =
    1 + sizeM m1 + sizeM m2

objetosM :: Mapa -> [Objeto]
objetosM FinM = []
objetosM (Bifurcacion o m1 m2) =
    [o] ++ objetosM m1 ++ objetosM m2

mapa1 :: Mapa
mapa1 = FinM

mapa2 :: Mapa
mapa2 = Bifurcacion Moneda FinM FinM

mapa3 :: Mapa
mapa3 = Bifurcacion Moneda
           (Bifurcacion Escudo FinM FinM)
           (Bifurcacion Arma FinM FinM)

mapa4 :: Mapa
mapa4 = Bifurcacion Escudo
            (Bifurcacion Moneda
                (Bifurcacion Arma FinM FinM)
                FinM)
            (Bifurcacion Arma FinM FinM)

mapa5 :: Mapa
mapa5 =
    Bifurcacion Escudo
        (Bifurcacion Moneda
            (Bifurcacion Arma FinM FinM)
            (Bifurcacion Escudo FinM FinM))
        (Bifurcacion Arma
            (Bifurcacion Moneda FinM FinM)
            FinM)

mapa6 :: Mapa
mapa6 = 
    Bifurcacion Escudo
        (Bifurcacion Moneda
            (Bifurcacion Arma FinM FinM)
            (Bifurcacion Escudo FinM FinM))
        (Bifurcacion Arma
            (Bifurcacion Moneda 
                FinM 
                (Bifurcacion Escudo FinM FinM))
            FinM)

mapa7 :: Mapa
mapa7 =
    Bifurcacion Escudo
        (Bifurcacion Moneda
            (Bifurcacion Arma 
                (Bifurcacion Escudo FinM FinM)
                FinM)
            (Bifurcacion Escudo FinM FinM))
        (Bifurcacion Arma
            (Bifurcacion Moneda 
                FinM 
                (Bifurcacion Escudo 
                    (Bifurcacion Arma FinM FinM) 
                    FinM))
            FinM)


cuantasArmas :: Mapa -> Int
cuantasArmas FinM = 0
cuantasArmas (Bifurcacion o m1 m2) =
    if esArma o
        then 1 + cuantasArmas m1 + cuantasArmas m2
        else cuantasArmas m1 + cuantasArmas m2

esArma :: Objeto -> Bool
esArma Arma = True
esArma _ = False

profundidadM :: Mapa -> Int
profundidadM FinM = 0
profundidadM (Bifurcacion o m1 m2) =
    1 + max (profundidadM m1)
            (profundidadM m2)

objetosCamMasLargo :: Mapa -> [Objeto]
objetosCamMasLargo FinM = []
objetosCamMasLargo (Bifurcacion o m1 m2) =
    if profundidadM m1 > profundidadM m2
        then o : objetosCamMasLargo m1
        else o : objetosCamMasLargo m2

objetosCamMasLargo' :: Mapa -> [Objeto]
objetosCamMasLargo' FinM = []
objetosCamMasLargo' (Bifurcacion o m1 m2) =
    if    length (objetosCamMasLargo' m1) 
        > length (objetosCamMasLargo' m2)
        then o : objetosCamMasLargo' m1
        else o : objetosCamMasLargo' m2

------------------------------------------

-- Las listas representan a todas las estructuras lineales

-- data [a] = []
--          | a : [a]

--  Vagon 12 (Vagon 20 (Vagon 30 Fin))
--  (12 : 20 : 30 : [])

--  (Mermelada "membrillo"
--     (Mermelada "ciruela" Pan))
--  ("membrillo" : "ciruela" : [])

------------------------------------------

-- arboles binarios
data Tree a = EmptyT
            | NodeT a (Tree a) (Tree a) deriving Show

mapaT :: Tree Objeto
mapaT =
    NodeT Escudo
        (NodeT Moneda
            (NodeT Arma EmptyT EmptyT)
            (NodeT Escudo EmptyT EmptyT))
        (NodeT Arma
            (NodeT Moneda EmptyT EmptyT)
            EmptyT)

tree2 :: Tree Int
tree2 =
    NodeT 123
        (NodeT 123
            (NodeT 123 EmptyT EmptyT)
            (NodeT 34 EmptyT EmptyT))
        (NodeT 234
            (NodeT 5 EmptyT EmptyT)
            (NodeT 1231 EmptyT EmptyT))

profundidadT :: Tree a -> Int
profundidadT EmptyT = 0
profundidadT (NodeT x ti td) =
    1 + max (profundidadT ti)
            (profundidadT td)

sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT x ti td) =
    x + sumarT ti + sumarT td

sucesoresT :: Tree Int -> Tree Int
sucesoresT EmptyT = EmptyT
sucesoresT (NodeT x ti td) =
    NodeT (x + 1)
          (sucesoresT ti)
          (sucesoresT td)

leaves :: Tree a -> [a]
leaves EmptyT = []
leaves (NodeT x ti td) =
    if esEmptyT ti && esEmptyT td
      then [x]
      else leaves ti ++ leaves td

    -- if esHoja (NodeT x ti td)
    --   then [x]
    --   else leaves ti ++ leaves td        

esEmptyT :: Tree a -> Bool
esEmptyT EmptyT = True
esEmptyT _      = False

leaves' :: Tree a -> [a]
leaves' EmptyT = []
leaves' (NodeT x EmptyT EmptyT) = [x]
leaves' (NodeT x ti td) =
    leaves' ti ++ leaves' td

esHoja :: Tree a -> Bool
esHoja (NodeT x EmptyT EmptyT) = True
esHoja _ = False
