-- | Módulo de implementación del tipo abstracto de datos árbol de búsqueda.
module EDA.Arbus (
	Arbus(ArbusVacio),
	inserta,
	consulta,
	esta,
	esVacio
) where

-- | 'Arbus' es un árbol de búsqueda con clave y valor.
--
--    Los elementos de tipo 'a' se denominarán «claves» y los del tipo 'b' «valores».
--    El tipo 'a' ha de ser ordenable (ser instancia de la clase 'Ord').
--
--    Se ha omitido la operación de borrado.
data Arbus a b =
	-- | Árbol de búsqueda vacío.
	ArbusVacio |
	-- | Construye un árbol de búsqueda a partir de unos hijos, una clave y un valor.
	Nodo (Arbus a b) a b (Arbus a b)

-- | Inserta un elemento en el árbol de búsqueda.
inserta :: (Ord a) => a -> b -> Arbus a b -> Arbus a b
inserta c v ArbusVacio 		= Nodo ArbusVacio c v ArbusVacio
inserta c v (Nodo iz c' v' dr)
	| c == c'		= Nodo iz c v dr
	| c < c'		= Nodo (inserta c v iz) c' v' dr
	| otherwise		= Nodo iz c' v' (inserta c v dr)

-- | Consulta el valor de una clave.
consulta :: (Ord a) => a -> Arbus a b -> Maybe b
consulta _ ArbusVacio	= Nothing
consulta c (Nodo iz c' v dr)
	| c == c'	= Just v
	| c < c'	= consulta c iz
	| otherwise	= consulta c dr

-- | Comprueba si una clave está en el árbol de búsqueda.
esta :: (Ord a) => a -> Arbus a b -> Bool
esta _ ArbusVacio 	= False
esta x (Nodo iz c _ dr)
	| x == c	= True
	| x < c		= esta x iz
	| otherwise	= esta x dr

-- | Comprueba si el árbol es vacío.
esVacio :: Arbus a b -> Bool
esVacio ArbusVacio	= True
esVacio _		= False

-- | Obtiene el recorrido en inorden del árbol, que se corresponde con
--   la lista de pares clave valor ordenada por clave.
inorden :: Arbus a b -> [(a, b)]
inorden ArbusVacio = []
inorden (Nodo iz c v dr) = inorden iz ++ (c, v):inorden dr

-- Igualdad por comparación de inórdenes
instance (Eq a, Eq b) => Eq (Arbus a b) where
	x == y = inorden x == inorden y

-- Muestra la lista de pares en inorden.
instance (Show a, Show b) => Show (Arbus a b) where
	show arb = "Arbus " ++ show (inorden arb)
