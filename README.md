# Intérprete del lenguaje REC

Intérprete de un lenguaje de definición y evaluación de funciones recursivas basado en el lenguaje *REC* del noveno capítulo (*Recursion equations*) del libro *[The Formal Semantics of Programming Languages: An Introduction](https://mitpress.mit.edu/books/formal-semantics-programming-languages)* de Glynn Winskel.

* Más información en la [wiki](https://github.com/ningit/rec-lang/wiki/Documentación).
* See the [wiki documentation](https://github.com/ningit/rec-lang/wiki/Documentation-(en)) for further information.
* Consultez la [wiki](https://github.com/ningit/rec-lang/wiki/Documentation-(fr)) pour en savoir plus.

## ¿Cómo compilar el programa?

Con [GHC](https://www.haskell.org/) y su biblioteca [Parsec](https://hackage.haskell.org/package/parsec) instalados, basta ejecutar `make` para generar el ejecutable `rec`. También es posible ejecutar el programa directamente con `runhaskell Main.lhs`.

La regla `doc` del *Makefile* genera la documentación del código con [Haddock](https://www.haskell.org/haddock/). La regla `info` genera la documentación presente en la wiki, si está disponible el programa [Pandoc](https://pandoc.org/).
