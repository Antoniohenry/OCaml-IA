# OCaml-IA

## How To Makefile 
https://caml.inria.fr/pub/old_caml_site/FAQ/Makefile_ocaml-fra.html

## Format des données
Les donnees sont des fichiers texte representant une grille rectangulaire avec 0 pour case vide, 1 pour case noire et la valeur de la lettre sinon \
0e0 \
10t \
010 

## Documentation
ocamldoc -html -d doc dico.mli status.mli reader.mli propagation.mli bt.mli main.ml

