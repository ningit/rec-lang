% Interprète de Rec

Ce logiciel est un interprète d'un langage de définition de fonctions récursives. Il se fonde sur le langage **Rec** défini dans le neuvième chapitre (*Recursion equations*) du livre *The Formal Semantics of Programming Languages: An Introduction* de Glynn Winksel.

Il faut marquer certaines particularités concernant essentiellement le besoin de concréter la présentation abstraite du langage :

* Des parenthèses sont admises pour éliminer l'ambiguïté de sa représentation textuelle. Ils peuvent s'employer pour enfermer n'importe quel terme. L'appel de fonction et la syntaxe pour la définir incluent de parenthèse aussi, qui ne peuvent pas s'omettre même si la fonction ne recevrait aucun paramètre.

* Les opérations arithmétiques sont associatives à gauche et le produit précède à l'addition. On peut mettre d'espace librement entre opérateurs, littéraux, variables, appels à fonctions...

* Pour facilité d'usage, l'opérateur préfixe de changement de signe (<code>-</code>) a été inclus. Les cas échéant, il peut s'éliminer sans problème.

* Les ensembles de variables numériques ou de fonction ne sont pas préétablis. Les identifiants de chaque genre de variable doivent être des paroles composées 
de lettres et nombres, dont le premier caractère soit une lettre.

* La représentation concrète des nombres utilise des entiers multiprécision (le type `Integer`{.haskell} de Haskell).

* La déclaration de fonctions peut s'intercaler avec l'évaluation de termes fermés. Si on essaie d'évaluer un terme contenant une fonction indéfinie, une erreur se produira.

* La déclaration répétée d'une même variable fonctionnelle écrasera la déclaration précédente, sans regarder à son arité.

* On a implémenté les deux sémantiques décrites dans le livre, avec passage de paramètre par valeur et par nom.


Fonctionnement de l'interprète
-----------------------------

L'interprète digère l'entrée de l'utilisateur ligne par ligne. Une ligne peut contenir une définition de fonction ou un terme fermé à être évalué. Chaque
ligne est indépendante sauf pour les définitions de fonctions qui augmentent le magasin de fonctions connues par le système.

Après l'évaluation satisfactoire d'un terme, le résultat obtenu sera montré. La déclaration réussie d'un fonction ne produise aucun texte.

Lors de la définition de fonctions, on peut faire référence à fonctions qui n'aient pas été définies auparavant, ce qui permet d'exprimer fonctions récursives et mutuellement récursives.

En cas d'erreur, un message informatif sera montré. Quelques erreurs possibles sont : erreur syntactique, variable inconnue, fonction inconnue ou fonction appelé avec une arité différente à celle de sa déclaration.

Lors que un terme fermé syntaxiquement correct mais pour lequel la sémantique n'est pas défini (il n'y pas une dérivation finie) est évalué, l'interprète se
bloquera. Le calcul peut s'interrompre en écrivant *Ctrl + C*.

Par défaut, les termes sont évalués avec la sémantique de passage de paramètre par valeur. Lors que le passage par nom est voulu, il faut l'indiquer avec l'option `-n` a la ligne de commande.


Recommandation pour l'utilisateur
---------------------------------

Pour simplicité, il n'y a pas de support pour la lecture de fichiers ou de facilités d'écriture dans le logiciel même.

Cependant il y a des solutions externes. Pour système type Unix:

* Des fichiers peuvent être redirigés à la sortie standard :
<code>./rec < fichier.rec</code>.
* On peut continuer à écrire après l'interprétation du fichier avec :
<code>cat archivo.rec - | ./rec</code>.
* Pour avoir la possibilité d'éditer les lignes et préserver l'historique de lignes introduites auparavant, on recommande l'utilisation du logiciel *rlwrap* : <code>rlwrap ./rec</code>


Exemples
--------

On ajoute des exemples exécutables bien connues :

* *Factorielle*. La fonction n'est pas définie sur les entiers négatifs, alors l'interprète se rendra en panne lors de l'évaluation d'un terme tel que `f(-1)`{.haskell}.

> ```haskell
> f(x) = if x then 1 else (x * f(x-1))
> ```

* *Fonction d'Ackermann*. Le calcul de `A(3, 9)`{.haskell} se fait attendre.

> ```haskell
> A(m, n)	= if m then n + 1 else (if n then A(m-1, 1) else A(m-1, A(m, n-1)))
> ```

* *Fonction signe* (exercice 9.1 du livre). `s` calcule le signe de son paramètre en utilisant la fonction auxiliaire `f`.

> ```haskell
> s(x)		= if x then 0 else f(x, 0 - x)
> f(x, y)	= if x then 1 else (if y then (0-1) else f(x-1, y-1))
> ```

En plus, on a ajouté un exemple `nombre.rec` où les sémantiques de passage par valeur et par nom diffèrent.

> ```haskell
> f(x) = f(x) + 1
> g(x) = 7
> g(f(2))
> ```

car l'évaluation de `g(f(2))`{.haskell} finit avec passage par nom mais pas avec passage par valeur, parce que ce dernier mode a besoin de l'évaluation de l'argument inutile `f(2)`{.haskell}, qui ne finit jamais.


Détails d'implémentation
--------------------------

Le logiciel est composé de divers modules. Dans le module `REC.Base`{.haskell}, on trouve les types de donnés pour l'arbre syntaxique abstrait de langage et le résultat de calculs, et l'arithmétique de ce derniers. Le module `REC.Sintaxis`{.haskell} est chargé de l'analyse syntactique et pour cela il recourt à la bibliothèque *Parsec* (version 3) de Haskell. Les modules `REC.PasoValor`{.haskell} et `REC.PasoNombre`{.haskell} contiennent les fonctions sémantiques de passage par valeur et passage par nom respectivement.

Ces modules utilisent une propre implémentation d'un arbre de recherche décrit dans `EDA.Arbus`{.haskell} comparable à `Data.Map`{.haskell}. Finalement le fichier `Main.lhs` s'occupe d'évaluer les lignes pris de l'entrée standard et montrer leurs résultats. Ici on fixe la fonction sémantique adéquate selon l'indication des paramètres.

Pour une information plus détaillée, le code et la documentation jointe est disponible en castillan.
