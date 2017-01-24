% Rec interpreter

This program is an interpreter for a recursion functions language. It is based on the **Rec** language defined in the ninth chapter (*Recursive equations*) of
Glynn Winskel's book *The Formal Semantics of Programming Languages: An Introduction*.

We must point out some particularities, mainly consequences of the concretion from the abstract language presentation in that book:

* Parenthesis can be used to remove the ambiguity of the language's textual representation. They can be used to enclose any term. Function call and function definition syntax include parenthesis, which cannot be omitted even if the functions does not receive any parameter.

* Arithmetic operations associate from the left and the product precede the sum. Spaces can be added freely between operators, literals, variables, function calls...

* For convenience, a prefix negation operator has been added (<code>-</code>). It can easily be removed if needed.

* The sets of numerical and functional variables are not fixed in advance, they are defined as the names are used. The identifier of both types of variables should be words composed of letters and numbers, whose first character is a letter.

* The actual representation of the numbers in the interpreter uses arbitrary precision integers (the Haskell's type `Integer`{.haskell}).

* Function declaration can be interleaved with term evaluation. When trying to evaluate a term using an undefined function name, an error will be produced.

* The repeated declaration of the same function variable will overwrite the previous definition, regardless of their arity.

* The two semantics describes in the book have been implemented in a denotational manner, call-by-value and call-by-name.


Interpreter behaviour
---------------------

The interpreter process the user input line by line, in a read-eval-print loop (REPL). A line can contain a function definition or a closed term to evaluate. Each line is independent except for the function definitions which make grow the store of functions known by the system.

After evaluating a term, the result will be shown. Successful function declarations do not produce any text.

Undefined names can be used at a function definition. That allows defining recursive and mutually recursive functions.

An informative message will be shown in case of error. Possible errors are: syntactic errors, unknown variable, unknown function or mistaken arity in a function call.

When a syntactically correct term for which the semantics are not defined is evaluated (the derivation process is not finite) the interpreter will hang. It can be interrupted by typing *Ctrl + C*.

By default, terms are evaluated using call-by-value semantics. If you want to use call-by-name, you must write the option `-n` as an argument to the program in the command line.


Recommendations to the user
---------------------------

For the sake of simplicity, no file reading or writing facilities have been incorporated.

However external solutions are available. In Unix-like systems:

* Files can be interpreted by a redirection to the standard input:
<code>./rec < file.rec</code>.
* Further writing can be done after interpreting the file:
<code>cat file.rec - | ./rec</code>.
* In order to have the ability to edit lines and maintain the history of inserted lines, the *rlwrap* program is recommended:
<code>rlwrap ./rec</code>


Examples
--------

Jointly with the interpreter some well known examples are included:

* *Factorial*. This function is not defined on the negative integers, so that the interpreter will hang if a term like `f(-1)`{.haskell} is evaluated.

> ```haskell
> f(x) = if x then 1 else (x * f(x-1))
> ```

* *Ackermann's function*. The calculation of `A(3, 9)`{.haskell} takes some time.

> ```haskell
> A(m, n)	= if m then n + 1 else (if n then A(m-1, 1) else A(m-1, A(m, n-1)))
> ```


* *Sign fuction* (exercise 9.1 of the book). `s` calculates its parameter's sign, using the auxiliary function `f`.

> ```haskell
> s(x)		= if x then 0 else f(x, 0 - x)
> f(x, y)	= if x then 1 else (if y then (0-1) else f(x-1, y-1))
> ```

An example `nombre.rec` has also been included to show that the call-by-value and call-by-name semantics may differ.

> ```haskell
> f(x) = f(x) + 1
> g(x) = 7
> g(f(2))
> ```

The evaluation of `g(f(2))`{.haskell} ends in the call-by-name mode but it does not in call-by-value, as this semantics requires the evaluation of the unneeded argument `f(2)`{.haskell}, which does not finish.


Implementation details
----------------------

The program is composed of various modules. In `REC.Base`{.haskell} module, data types are defined for the abstract syntax tree of the language and for the computation results along with its arithmetic. The module `REC.Sintaxis`{.haskell} is charged of the syntax analysis and for that purpose it makes use of the *Parsec* Haskell's library (version 3). Modules  `REC.PasoValor`{.haskell} and `REC.PasoNombre`{.haskell} contain the semantic functions for call-by-value and call-by-name.

These modules use a custom implementation of binary search trees described in `EDA.Arbus`{.haskell} and which can be compared to `Data.Map`{.haskell}. Finally, the `Main.lhs` file evaluates lines from the standard input and shows their results. Here the semantic function is selected according to the program parameters.

For further information, the code and joint documentation is available in Spanish.
