# Makefile para el intérprete de REC

PANDOC	:= $(shell pandoc -v 2> /dev/null)
FUENTES := $(shell find . -type f -name '*.hs' -or -name '*.lhs')

.PHONY: info clean clean-doc doc

# Compilar con GCH
make:
	ghc -O Main.lhs -fno-warn-tabs -o rec

# Eliminar tabulaciones en los archivos fuente
destab:
	$(foreach f, ${FUENTES}, expand ${f} > ${f}.expanded; mv ${f}.expanded ${f};)

# Generar la documentación a nivel de código (con Haddock)
doc:
	haddock -h --optghc=-fno-warn-tabs -o doc/ref Main.lhs -t "Ecuaciones recursivas - Lenguaje Rec"

# Generar la documentación adjunta
ifdef PANDOC
info: Main.htm doc/manual.es.htm doc/manual.en.htm doc/manual.fr.htm

else
info:
	$(error Se necesita Pandoc para generar la documentación)
endif

%.htm: %.md
	pandoc --from=markdown -s --to=html5 $< -o $@

%.htm: %.lhs
	pandoc --from=markdown+lhs -s --to=html5 $< -o $@

# Borrar archivos intermedios generados por GHC
clean:
	$(RM) REC/*.hi EDA/*.hi REC/*.o EDA/*.o Main.hi Main.o

# Borra la documentación generada 
clean-doc:
	$(RM) -r doc/ref
	$(RM) Main.htm doc/*.htm
