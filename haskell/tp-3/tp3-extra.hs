-- 1. Biblioteca con libros prestados
-- Representaremos una biblioteca sencilla donde cada libro puede estar prestado o
-- disponible.
data Estado = Disponible | Prestado
data Biblioteca = Libro Estado Biblioteca | BibliotecaVacia 

-- En esta representación, cada aparición de Libro representa un libro dentro de la
-- biblioteca.
-- Por ejemplo, una biblioteca con 2 libros prestados y 1 disponible 
-- podría representarse así:
-- Libro Prestado (Libro Disponible (Libro Prestado BibliotecaVacia))
-- Implementar las siguientes funciones sobre bibliotecas:

------------------------------------------------------------------------------------------
-- Dado un estado y una biblioteca, devuelve cuántos libros se encuentran en dicho estado.
-- Ejemplo:
-- contarEstado Prestado biblioteca
-- debería devolver la cantidad de libros prestados.
-- Nota: pensar si alguna función sobre listas puede ayudar a resolver el problema

esElMismoEstado :: Estado -> Estado -> Bool  -- Funcion auxiliar
esElMismoEstado Disponible Disponible = True
esElMismoEstado Prestado Prestado     = True
esElMismoEstado _ _                   = False

contarEstado :: Estado -> Biblioteca -> Int
contarEstado e BibliotecaVacia = 0
contarEstado e (Libro el b)    = if esElMismoEstado e el 
								  then 1 + contarEstado e b
								  else contarEstado e b
								  
-------------------------------------------------------------------------------------------
-- Dado un estado y una biblioteca, agrega un nuevo libro con dicho estado a la bilioteca.
-- Ejemplo:
-- agregarLibro Disponible biblioteca
-- agrega un libro disponible.

agregarLibro :: Estado -> Biblioteca -> Biblioteca
agregarLibro e b = Libro e b

-------------------------------------------------------------------------------------------
-- Quita un libro prestado de la biblioteca, simulando que fue devuelto.
-- Si no hay libros prestados, la biblioteca debe permanecer igual.
-- Importante: la función debe quitar solamente un libro prestado.

devolverLibro :: Biblioteca -> Biblioteca
devolverLibro BibliotecaVacia = BibliotecaVacia
devolverLibro (Libro e b) 	  = if esElMismoEstado e Prestado
								 then agregarLibro Disponible b
								 else agregarLibro e (devolverLibro b)

-------------------------------------------------------------------------------------------
-- Dado un número n, un estado y una biblioteca, agrega n libros con dicho estado.
-- Ejemplo:
-- agregarVarios 3 Disponible biblioteca
-- agrega 3 libros disponibles.

agregarVarios :: Int -> Estado -> Biblioteca -> Biblioteca
agregarVarios 0 e b = b
agregarVarios n e b = agregarLibro e (agregarVarios (n - 1) e b)

-------------------------------------------------------------------------------------------
-- Dado un estado inicial y otro final, reemplaza todos los libros del
-- primer estado por el segundo.
-- Ejemplo:
-- cambiarEstado Prestado Disponible biblioteca
-- marca todos los libros prestados como disponibles.

cambiarEstado :: Estado -> Biblioteca -> Biblioteca
cambiarEstado e1 BibliotecaVacia = BibliotecaVacia
cambiarEstado e1 (Libro e2 b) 	 = if not (esElMismoEstado e1 e2)
								    then agregarLibro e1 (cambiarEstado e1 b)
									else agregarLibro e2 (cambiarEstado e1 b)
									
--------------------------------------------------------------------------------------------
-- 2. Playlist de música
-- Representaremos una playlist de canciones marcadas como favoritas o normales.
data TipoCancion = Favorita | Normal
data Playlist = Cancion TipoCancion Playlist | PlaylistVacia

-- Por ejemplo:
-- Cancion Favorita (Cancion Normal (Cancion Favorita PlaylistVacia))
-- representa una playlist con dos canciones favoritas y una normal.
-- Implementar las siguientes funciones:

---------------------------------------------------------------------------------------------
-- Cuenta cuántas canciones de un determinado tipo existen en la playlist.

mismoTipo :: TipoCancion -> TipoCancion -> Bool -- Funcion auxiliar
mismoTipo Favorita Favorita = True
mismoTipo Normal Normal		= True
mismoTipo _ _				= False

cantidadTipo :: TipoCancion -> Playlist -> Int
cantidadTipo _ PlaylistVacia = 0
cantidadTipo t (c tc pl)	 = if mismoTipo t tc
								then 1 + cantidadTipo pl
								else cantidadTipo pl

---------------------------------------------------------------------------------------------
agregarCancion :: TipoCancion -> Playlist -> Playlist
-- Agrega una canción del tipo indicado a la playlist. 
agregarCancion t pl = Cancion t pl

----------------------------------------------------------------------------------------------
eliminarFavorita :: Playlist -> Playlist
-- Elimina una canción marcada como favorita.
-- Si no existen favoritas, la playlist debe permanecer igual.

esFavorita :: TipoCancion -> Bool  -- Funcion auxiliar
esFavorita Favorita = True
esFavorita Normal	= False

eliminarFavorita PlaylistVacia = PlaylistVacia
eliminarFavorita (Cancion tc pl) = if esFavorita tc
								    then pl
								    else agregarCancion tc (eliminarFavorita pl)

----------------------------------------------------------------------------------------------
-- Duplica cada canción favorita encontrada.
-- Ejemplo:
-- Favorita, Normal
-- debería transformarse en:
-- Favorita, Favorita, Normal

duplicarFavoritas :: Playlist -> Playlist
duplicarFavoritas PlaylistVacia	  = PlaylistVacia
duplicarFavoritas (Cancion tc pl) = if esFavorita tc
									 then agregarCancion tc (agregarCancion tc  (duplicarFavoritas pl))
									 else agregarCancion tc (duplicarFavoritas pl)

-----------------------------------------------------------------------------------------------
-- Invierte el orden de las canciones de la playlist.

-- invertir :: [a] -> [a]
-- invertir []     = []
-- invertir (x:xs) = invertir xs ++ [x]
-----------------------------------------
-- invertir :: Listita a -> Listita a
-- invertir Nil 		= Nil 
-- invertir (cons x xs) = unirListita (invertir xs) (cons x Nil)


-- ++ :: [a] -> [a] -> [a]
-- ++ xs [] 	= xs
-- ++ xs (y:ys) = ++ (xs ++ [y]) ys
-----------------------------------------
-- ul :: Listita a -> Listita a -> Listita a
-- ul l Nil 		= l
-- ul l (Cons y ys) = ul (ul l (Cons y Nil)) ys
-----------------------------------------
-- up :: Playlist -> Playlist -> Playlist
-- up pl PlaylistVacia 	  = pl
-- up pl (Cancion tc pl2) = up (up pl (agregar tc PlaylistVacia)) pl2

unirPlaylists :: Playlist -> Playlist -> Playlist   -- SUBFUNCION --
unirPlaylists pl (Cancion tc pl2) = 				----------------
		unirPlaylists -- ++
		(unirPlaylists pl (agregarCancion tc PlaylistVacia)) -- (xs ++ [y])
		pl2 -- ys

invertirPlaylist :: Playlist -> Playlist
invertirPlaylist PlaylistVacia   = PlaylistVacia
invertirPlaylist (Cancion tc pl) = unirPaylists (invertirPlaylist pl) (agregarCancion tc PlaylistVacia)

------------------------------------------------------------------------------------------------
-- Indica si todas las canciones son favoritas.
-- Importante: decidir qué ocurre con la playlist vacía.
todasFavoritas :: Playlist -> Bool
todasFavoritas PlaylistVacia   = True
todasFavoritas (Cancion tc pl) = if esFavorita tc
								  then todasFavoritas pl
								  else False

------------------------------------------------------------------------------------------------
-- 3. Carrito de compras
-- Representaremos un carrito donde los productos pueden ser alimentos, electro o limpieza.
data Categoria = Alimento | Limpieza | Electrodomestico
data Carrito = Producto Categoria Carrito | CarritoVacio

-- Ejemplo:
-- Producto Alimento (Producto Limpieza (Producto Electrodomestico CarritoVacio))
-- Implementar las siguientes funciones:

-------------------------------------------------------------------------------------------------
-- Cuenta cuántos productos pertenecen a cierta categoría.

esMismaCategoria :: Categoria -> Categoria -> Bool
esMismaCategoria Alimento Alimento 				   = True
esMismaCategoria Limpieza Limpieza 				   = True
esMismaCategoria Electrodomestico Electrodomestico = True
esMismaCategoria _ _							   = False

contarCategoria :: Categoria -> Carrito -> Int
contarCategoria cat (Producto catp car) = if esMismaCategoria cat catp
											then 1 + (contarCategoria cat car)
											else contarCategoria cat car

-------------------------------------------------------------------------------------------------
--Agrega un producto al carrito.
agregarProducto :: Categoria -> Carrito -> Carrito
agregarProducto cat car = Producto cat car

-------------------------------------------------------------------------------------------------
-- Quita un producto de la categoría indicada.
-- La función debe quitar solamente el primero que encuentre.
quitarProducto :: Categoria -> Carrito -> Carrito
quitarProducto cat CarritoVacio = CarritoVacio
quitarProducto cat (Producto catp car) = if (esMismaCategoria cat catp)
											then car
											else agregarProducto catp (quitarProducto cat car))

-------------------------------------------------------------------------------------------------
-- Elimina todos los productos de una categoría determinada.
-- Ejemplo:
-- vaciarCategoria Limpieza carrito
-- elimina todos los productos de limpieza.
vaciarCategoria :: Categoria -> Carrito -> Carrito
vaciarCategoria cat CarritoVacio = CarritoVacio
vaciarCategoria cat (Producto catp car) = if (esMismaCategoria cat catp)
											then vaciarCategoria cat car
											else agregarProducto catp (vaciarCategoria cat)

--------------------------------------------------------------------------------------------------
-- Indica si el carrito contiene al menos un producto.
hayProductos :: Carrito -> Bool
hayProducto CarritoVacio = False
hayProducto _			 = True

--------------------------------------------------------------------------------------------------
-- Combina dos carritos en uno solo.
-- Importante: conservar todos los productos de ambos carritos.
mezclarCarritos :: Carrito -> Carrito -> Carrito
mezclarCarritos car CarritoVacio = car
mezclarCarritos car1 (Producto cat car2) = 
					mezclarCarritos (mezclarCarritos car1 (agregarProducto cat CarritoVacio)) car2

---------------------------------------------------------------------------------------------------
-- 4. Historial de mensajes
-- Representaremos un historial de mensajes donde cada mensaje puede ser 
-- leído o no leído.
data EstadoMensaje = Leido | NoLeido
data Chat = Mensaje EstadoMensaje Chat | ChatVacio

-- Ejemplo:
-- Mensaje NoLeido (Mensaje Leido (Mensaje NoLeido ChatVacio))
-- Implementar las siguientes funciones:

----------------------------------------------------------------------------------------------------
-- Cuenta cuántos mensajes tienen cierto estado.

esMismoEstado :: EstadoMensaje -> EstadoMensaje -> Bool
esMismoEstado Leido Leido	  = True
esMismoEstado NoLeido NoLeido = True
esMismoEstado _ _			  = False

contarMensajes :: EstadoMensaje -> Chat -> Int
contarMensajes em ChatVacio 		 = 0
contarMensajes em (Mensaje emc chat) = if esMismoEstado
										then 1 + (contarMensajes em chat)
										else contarMensajes em chat

----------------------------------------------------------------------------------------------------
-- Agrega un mensaje nuevo al chat.
recibirMensaje :: EstadoMensaje -> Chat -> Chat
recibirMensaje em chat = Mensaje em chat

----------------------------------------------------------------------------------------------------
-- Marca como leído el primer mensaje no leído que aparezca.
-- Si todos están leídos, el chat permanece igual.

estaLeido :: EstadoMensaje -> Bool
estadoLeido Leido   = True
estadoLeido NoLeido	= False

leerMensaje :: Chat -> Chat
leerMensaje ChatVacio 		   = ChatVacio
leerMensaje (Mensaje em chat) = if estaLeido em
								   then recibirMensaje em (leerMensajes chat)
								   else recibirMensaje Leido chat

----------------------------------------------------------------------------------------------------
-- Marca todos los mensajes como leídos.
marcarTodos :: Chat -> Chat
marcarTodos ChatVacio 		   = ChatVacio
marcarTodos (Mensaje em chat) = recibirMensaje Leido (marcarTodos chat)
								   
-----------------------------------------------------------------------------------------------------
-- Indica si existe al menos un mensaje sin leer.
hayNoLeidos :: Chat -> Bool
hayNoLeidos ChatVacio 		  = False
hayNoLeidos (Mensaje em chat) = if estadoLeido em
								  then hayNoLeidos chat
								  else True

------------------------------------------------------------------------------------------------------
-- Elimina todos los mensajes ya leídos del historial.
borrarLeidos :: Chat -> Chat
borrarLeidos ChatVacio 		   = ChatVacio
borrarLeidos (Mensaje em chat) = if estadoLeido em
									then borrarLeidos chat
									else recibirMensaje em (borrarLeidos chat)
