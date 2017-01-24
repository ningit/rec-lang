-----------------------------------------------------------------------------
-- |
-- Module      :  REC.PasoValor
--
-- Función semántica de paso por valor.
-- 
-----------------------------------------------------------------------------

module REC.PasoValor (
	valor
) where

import REC.Base
import EDA.Arbus

-- | Función semántica de paso por valor,
-- acorde a la definición del capítulo 9 de
-- /Formal Semantics of Programming Languages/ de Glynn Winksel.

valor	::	Arbus String FuncionRec 	-- ^ Entorno de funciones
		-> ExpresionRec -> Resultado

valor 		= evaluar ArbusVacio


-- | Definición completa de la función semántica.

evaluar ::	Arbus String Integer		-- ^ Entorno de variables (a valores enteros)
		-> Arbus String FuncionRec 	-- ^ Entorno de funciones
		-> ExpresionRec -> Resultado

evaluar _ _	(Literal m)		= Valor m
evaluar vs fs	(Suma izq dr)		= evaluar vs fs izq + evaluar vs fs dr
evaluar vs fs	(Resta izq dr)		= evaluar vs fs izq - evaluar vs fs dr
evaluar vs fs	(Producto izq dr)	= evaluar vs fs izq * evaluar vs fs dr
evaluar vs fs	(Condicional cn rt rf)
	| not (esValor cond)		= cond
	| cond == (Valor 0)		= evaluar vs fs rt
	| otherwise			= evaluar vs fs rf
	where cond = evaluar vs fs cn

evaluar vs _	(Variable id)		= comoRes $ consulta id vs
	where	comoRes (Just n)	= (Valor n)
		comoRes Nothing		= ENoVar id

evaluar vs fs	(Llamada id ps)	= evaluarAux (consulta id fs)
	where	evaluarAux :: Maybe FuncionRec -> Resultado

		evaluarAux Nothing			= ENoFunc id
		evaluarAux (Just (args, cuerpo))
			| length ps == length args	= case evParams args (map (evaluar vs fs) ps) of
				Right vs'		-> evaluar vs' fs cuerpo
				Left res		-> res 
			| otherwise			= EAridad id (length ps) (length args)

		evParams  :: [String] -> [Resultado] -> Either (Resultado) (Arbus String Integer)

		evParams (n:ns) ((Valor r):rs)	= (evParams ns rs) >>= (return . (inserta n r))
		evParams _ (r:_)		= Left r
		evParams _ []			= Right ArbusVacio
