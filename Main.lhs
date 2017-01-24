% Intérprete de Rec

Para documentación general y sobre uso del programa véase la documentación
adjunta separada.

Detalles de implementación
--------------------------

El intérprete se ha dividido en diversos módulos que se encargan de distintas
tareas, como generar el árbol sintáctico abstracto a partir de la sintaxis
concreta o evaluar semánticamente dicho árbol.

> import EDA.Arbus
> import REC.Base
> import REC.Sintaxis
> import qualified REC.PasoValor as Valor
> import qualified REC.PasoNombre as Nombre
>
> import System.Environment

Para el análisis sintáctico se ha utilizado la biblioteca de Haskell
*Parsec* (versión 3).

> import Text.Parsec (parse)
> import Text.Parsec.String (Parser)

El estado viene representado por un árbol de búsqueda binario que asocia nombres
de función a pares lista de variables-término, que es la representación escogida
para las funciones de Rec.

> -- | Estado del intérprete de Rec.
> type EstadoRec = Arbus String FuncionRec

Inicialmente no hay función alguna predefinida.

> -- | Estado inicial
> estadoInicial :: EstadoRec
> estadoInicial = ArbusVacio

> -- | Parseador de términos cerrados
> tcerrado :: Parser ExpresionRec
> tcerrado = termino []


La función principal lee texto de la entrada estándar y muestra el resultado en
la salida por defecto.

> -- | Alias para el tipo de las funciones semánticas
> type FnSemantica = Arbus String FuncionRec -> ExpresionRec -> Resultado
>
> -- | Función principal
> main :: IO ()
> main = do
>	espec		<- getContents
>	fn		<- averiguaFn getArgs
>	mapM_ putStrLn (interpretaLinea fn estadoInicial $ lines espec)
>
> 	where	averiguaFn :: IO [String] -> IO FnSemantica
>
>		averiguaFn = fmap (\x -> if x then Nombre.valor else Valor.valor)
>				. fmap (any (\arg -> arg == "-n"))

El programa intentará interpretar cada línea en primer lugar como una declaración de
función. En caso de error se intentará interpretar como término cerrado.

> interpretaLinea :: FnSemantica -> EstadoRec -> [String] -> [String]
> interpretaLinea _ _ []		= []
> interpretaLinea fn st (x:xs) 	= either (\_ -> term:(interpretaLinea fn nst xs))
>					 (\_ -> interpretaLinea fn nst xs) decl
> 	where	decl	= parse declaracion "" x
> 		term	= interpretaTermino fn st x
> 		nst	= either (\_ -> st) (\(nombre, def) -> inserta nombre def st) decl

Esta función analiza sintácticamente la expresión dada como un término cerrado
y devuelve el resultado de evaluarla o un mensaje de error apropiado.
 
> -- | Intenta parsear un término
> interpretaTermino :: FnSemantica -> EstadoRec -> String -> String
> interpretaTermino fn st exp = either (show) (evaluaTermino fn st) (parse tcerrado "" exp)

Esta función evalúa semánticamente un término, mostrando el mensaje de error apropiado
en caso de fallo.

> -- | Evalúa (semánticamente) un término ya parseado y convierte la salida
> evaluaTermino :: FnSemantica -> EstadoRec -> ExpresionRec -> String
> evaluaTermino fn st exp = case (fn st exp) of
> 	Valor v			-> show v
> 	ENoVar id		-> "Error: variable desconocida «" ++ id ++ "»."
> 	ENoFunc id		-> "Error: función desconocida «" ++ id ++ "»."
> 	EAridad id ao ad	-> "Error: aridad incorrecta en la función «" ++
>		id ++ "» (" ++ show ao ++ " en lugar de " ++ show ad ++ ")."
