-----------------------------------------------------------------------------
-- |
-- Module      :  REC.Sintaxis
--
-- Módulo de interpretación sintáctica del lenguaje Rec.
--
-- Sigue la sintaxis establecida en el capítulo 9 de
-- /Formal Semantics of Programming Languages/ de Glynn Winskel,
-- con las particularidades que se indiquen.
--
-- Todos los parseadores ignoran los carácteres extra a la derecha de una
-- expresión.
-- 
-----------------------------------------------------------------------------

module REC.Sintaxis (
	termino,
	declaracion
) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Control.Applicative((<$>))
import REC.Base

------
-- Definiciones generales
------

-- | Número (en decimal)
numero :: Parser Integer
numero = do
	n	<- many1 digit
	spaces
	return (read n)

-- | Indentificador de variables y nombres de función
identif :: Parser String
identif = do
	cab <- letter
	col <- many alphaNum
	spaces
	return (cab:col)

-- | Signatura de una función
signatura :: Parser [String]
signatura = sepBy identif separador
	where separador = do
		_ <- char ','
		spaces

-- | Parámetros de llamada a función
parametros :: [String] -> Parser [ExpresionRec]
parametros vs = sepBy (termino vs) separador
	where separador = do
		_ <- char ','
		spaces

-- | Operador binario (incluye el espacio consecutivo)
opbinario :: Char -> Parser Char
opbinario c = do
	_ <- char c
	spaces
	return c

------
-- Definiciones de elementos del lenguaje
------

-- | Literal numérico
literal :: Parser ExpresionRec
literal = Literal <$> numero


-- | Variable
variable :: [String] -> Parser ExpresionRec
variable vs = do
	id <- identif
	variableAux id
	where	variableAux :: String -> Parser ExpresionRec
		variableAux id 
			| id `elem`vs	= return (Variable id)
			| otherwise	= fail "unexpected variable name"

-- | Llamada a función
llamada :: [String] -> Parser ExpresionRec
llamada vs = do
	id	<- identif
	_	<- char '('
	ps	<- parametros vs
	_	<-char ')'
	spaces
	return (Llamada id ps)

-- | Paréntesis
parentesis :: [String] -> Parser ExpresionRec
parentesis vs = do
	_	<- char '('
	e	<- termino vs
	_	<- char ')'
	spaces
	return e

-- | Condicional
condicional :: [String] -> Parser ExpresionRec
condicional vs = do
	_	<- string "if"
	spaces
	cn	<- termino vs
	_	<- string "then"
	spaces
	rt	<- termino vs
	_	<- string "else"
	spaces
	rf	<- termino vs
	spaces
	return (Condicional cn rt rf)


------
-- Definiciones finales
------

-- | Expresiones básicas del lenguaje
termBase :: [String] -> Parser ExpresionRec
termBase vs = literal <|> try (condicional vs) <|> try (llamada vs) <|> variable vs <|> parentesis vs

-- | Tabla de operadores del lenguaje
tablaOp = [
		[Prefix (char '-' >> return (Resta (Literal 0)))],
		[Infix (opbinario '*' >> return Producto) AssocLeft],
		[Infix (opbinario '+' >> return Suma) AssocLeft,
			Infix (opbinario '-' >> return Resta) AssocLeft]
	  ]

-- | Analiza sintácticamente un término del lenguaje Rec.
--
-- Permite la utilización de paréntesis, espacios entre términos, considera
-- la precedencia del operador producto y admite - como operador prefijo.

termino :: [String] -> Parser ExpresionRec
termino vs = buildExpressionParser tablaOp (termBase vs)

-- | Analiza sintácticamente una declaración simple de función.
--
-- Asegura (a nivel sintáctico) que las variables empleadas en la definición
-- del término están incluidas en la definición formal de variables anterior.

declaracion :: Parser (String, FuncionRec)
declaracion = do
	id	<- identif
	_	<- char '('
	vars	<- signatura
	_	<- char ')'
	spaces
	_	<- char '='
	spaces
	t	<- termino vars
	return (id, (vars, t))
