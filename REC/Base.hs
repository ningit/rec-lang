-----------------------------------------------------------------------------
-- |
-- Module      :  REC.Base
--
-- Definiciones básicas de Rec.
--
-- Sintaxis abstracta y tipo del valor semántico.
-- 
-----------------------------------------------------------------------------

module REC.Base (
	ExpresionRec(..),
	FuncionRec,
	Resultado(..),
	esValor
) where

-- | Representación abstracta de un término de Rec.
data ExpresionRec =
	Literal Integer |
	Variable String |
	Suma ExpresionRec ExpresionRec |
	Resta ExpresionRec ExpresionRec |
	Producto ExpresionRec ExpresionRec |
	Condicional ExpresionRec ExpresionRec ExpresionRec |
	Llamada String [ExpresionRec]
	deriving (Show)

-- | Definición de función del lenguaje Rec.
--
-- Es un par con la lista de nombres de variables y el
-- término que constituye su definición.

type FuncionRec = ([String], ExpresionRec)

-- | Resultado de la evaluación de un término Rec.
--
-- Desglosa las posibles causas de error como sigue:
data Resultado =
	-- | Resultado correcto
	Valor Integer |
	-- | Referencia a una función desconocida
	ENoFunc String |
	-- | Referencia a una variable desconocida
	ENoVar String |
	-- | Llamada a función con una aridad diferente de la de su definición
	-- (se indican por ese orden)
	EAridad String Int Int
	deriving (Show, Eq)

-- | Aritmética numérica por comodidad.
-- En caso de error prevalece el generado en el término más a la izquierda.
instance Num Resultado where
	(Valor n) + (Valor m)	= Valor (n + m)
	(Valor _) + res		= res
	res + _			= res

	(Valor n) * (Valor m)	= Valor (n * m)
	(Valor _) * res		= res
	res * _			= res

	abs (Valor n) 		= Valor (abs n)
	abs res			= res

	signum (Valor n)	= Valor (signum n)
	signum _		= Valor 1

	fromInteger n		= Valor n

	negate (Valor n)	= Valor (negate n)
	negate res		= res

-- | Detecta si un resultado es correcto (es decir, no es un error)
esValor :: Resultado -> Bool
esValor (Valor _)	= True
esValor	_		= False
