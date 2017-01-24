% Intérprete de Rec

Este programa es un intérprete de un lenguaje de definición de funciones recursivas sobre los números enteros. Está basado en el lenguaje **Rec** definido en el noveno capítulo (*Recursion equations*) del libro *The Formal Semantics of Programming Languages: An Introduction* de Glynn Winskel.

Presenta diversas particularidades surgidas en su mayor parte de la necesidad de concretar la presentación abstracta del lenguaje:

* Se admiten paréntesis para eliminar la ambigüedad de su representación textual. Estos pueden encerrar cualquier término. La llamada a función y la sintaxis para definirlas también incluye paréntesis, que no se pueden omitir ni siquiera cuando la función no reciba parámetros.

* Las operaciones aritméticas asocian por la izquierda y el producto precede a la suma. Entre operadores, literales, variables, llamadas a función... se pueden añadir espacios libremente.

* Por comodidad se ha añadido el operador prefijo de cambio de signo (<code>-</code>). En caso de necesidad se puede eliminar sin menor problema.

* Los conjunto de variables numéricas y de función no están preestablecidos, se construyen con el uso. Los identificadores de ambos tipos de variables han
de ser palabras compuestas por letras y números, cuyo primer carácter sea una letra.

* La representación concreta de los números en el intérprete utiliza enteros de precisión arbitraria (el tipo `Integer`{.haskell} de Haskell).

* La declaración de funciones se puede intercalar con la evaluación de términos cerrados. Si se intenta evaluar un término que utilice una función sin
definir se producirá un error.

* Declaraciones sucesivas de una misma función sobrescribirán las definiciones anteriores, independientemente de su aridad.

* Se han implementado al estilo denotacional las dos semánticas descritas en el libro, con paso de parámetros por valor y por nombre.


Funcionamiento del intérprete
-----------------------------

El intérprete procesa la entrada del usuario línea a línea, en modo lectura-evaluación-impresión (REPL). Una línea puede contener una definición de función o un término cerrado a evaluar. Cada línea es independiente salvo por las definiciones de función, que se añaden al almacén de funciones disponibles en el sistema.

Tras evaluar satisfactoriamente un término se mostrará el resultado obtenido. La declaración exitosa de funciones no produce texto alguno.

Al definir funciones se pueden utilizar funciones que aún no hayan sido definidas, lo que permite expresar funciones recursivas y mutuamente recursivas.

En caso de error se mostrará un mensaje informativo. Algunos de los posibles errores pueden ser: errores sintácticos, variable desconocida, función desconocida o función llamada con una aridad distinta a la de su declaración.

En caso de evaluar un término sintácticamente correcto para el que no esté definida la semántica (no exista una derivación finita) el intérprete quedará colgado. Se puede interrumpir el cómputo introduciendo *Ctrl + C*.

Por defecto los términos se evalúan respecto a la semántica de paso de parámetros por valor. Si se quiere utilizar paso por nombre es preciso indicarlo
mediante la opción `-n` como argumento al programa en la línea de comandos.


Recomendaciones de uso
----------------------

Por simplicidad no se ha añadido soporte para lectura de archivos ni facilidades de escritura.

No obstante existen soluciones externas. En sistemas tipo Unix:

* Se pueden interpretar archivos redirigiéndolos a la entrada estándar: `./rec < archivo.rec`{.bash}.
* Se puede continuar escribiendo tras interpretar el archivo utilizando: `cat archivo.rec - | ./rec`{.bash}.
* Para tener la posibilidad de editar las líneas y conservar el historial de líneas introducidas se recomienda utilizar el programa *rlwrap*: `rlwrap ./rec`{.bash}.


Ejemplos
--------

Adjuntos al programa se incluyen algunos ejemplos bien conocidos:

* *Factorial*. La función no está definida sobre los enteros negativos, así que el intérprete se quedará colgado si se evalúa por ejemplo `f(-1)`{.haskell}.

> ```haskell
> f(x) = if x then 1 else (x * f(x-1))
> ```

* *Función de Ackermann*. El cálculo de `A(3, 9)`{.haskell} se hace esperar.

> ```haskell
> A(m, n)	= if m then n + 1 else (if n then A(m-1, 1) else A(m-1, A(m, n-1)))
> ```

* *Función signo* (ejercicio 9.1 del libro). `s` calcula el signo de su parámetro, utilizando la función auxiliar `f`.

> ```haskell
> s(x)		= if x then 0 else f(x, 0 - x)
> f(x, y)	= if x then 1 else (if y then (0-1) else f(x-1, y-1))
> ```

Además se ha incluido un ejemplo `nombre.rec` en el que la semántica de paso por valor y la de paso por nombre difieren

> ```haskell
> f(x) = f(x) + 1
> g(x) = 7
> g(f(2))
> ```

pues la evaluación de `g(f(2))`{.haskell} termina en paso por nombre pero no en paso por valor, pues este último modo requiere la evaluación del argumento  innecesario `f(2)`{.haskell}, que no termina.


Detalles de implementación
--------------------------

El programa consta de diversos módulos. En el módulo `REC.Base`{.haskell} se definen los tipos de datos para el árbol de sintaxis abstracta del lenguaje y el resultado de los cómputos junto con su aritmética. El módulo `REC.Sintaxis`{.haskell} es el encargado del análisis sintáctico y para ello utiliza la biblioteca *Parsec* (version 3) de Haskell. A su vez los módulos `REC.PasoValor`{.haskell} y `REC.PasoNombre`{.haskell} contienen las funciones semánticas de paso por valor y paso por nombre respectivamente.

Estos módulos utilizan una implementación propia de un árbol binario de búsqueda descrito en `EDA.Arbus`{.haskell} comparable al de `Data.Map`{.haskell}. Finalmente el archivo `Main.lhs` se encarga de evaluar las líneas tomadas de la entrada estándar y mostrar los resultados. Aquí se selecciona la función semántica adecuada según se indique en los parámetros.

Para información más detallada véase el código y la documentación generada junto a él.
