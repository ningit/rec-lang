-----------------------------------------------------------------------------
-- |
-- Module      :  REC.PasoNombre
--
-- Función semántica de paso por nombre.
--
-- La definición de la función semántica es prácticamente igual a la de paso
-- por valor. Los errores de función desconocida en los parámetros que no
-- lleguen a evaluarse pasarán desapercibidos.
--
-----------------------------------------------------------------------------

module REC.PasoNombre (
	valor
) where

import REC.Base
import EDA.Arbus

-- | Función semántica de paso por nombre,
-- acorde a la definición del capítulo 9 de
-- /Formal Semantics of Programming Languages/ de Glynn Winksel.

valor	::	Arbus String FuncionRec 	-- ^ Entorno de funciones
		-> ExpresionRec -> Resultado

valor 		= evaluar ArbusVacio


-- | Definición completa de la función semántica.

evaluar ::	Arbus String Resultado		-- ^ Entorno de variables (a resultados)
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

evaluar vs _	(Variable id)			= comoRes $ consulta id vs
	where	comoRes (Just res)	= res
		comoRes Nothing		= ENoVar id

evaluar vs fs	(Llamada id ps)	= evaluarAux (consulta id fs)
	where	evaluarAux :: Maybe FuncionRec -> Resultado

		evaluarAux Nothing			= ENoFunc id
		evaluarAux (Just (args, cuerpo))
			| length ps == length args	= evaluar nuevoVar fs cuerpo
			| otherwise			= EAridad id (length ps) (length args)

			where nuevoVar = evParams args (map (evaluar vs fs) ps)

		evParams  :: [String] -> [Resultado] -> Arbus String Resultado

		evParams (n:ns) (r:rs)	= inserta n r (evParams ns rs)
		evParams _ []		= ArbusVacio
