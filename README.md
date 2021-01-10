# OCaml-IA

## How To Makefile 
https://caml.inria.fr/pub/old_caml_site/FAQ/Makefile_ocaml-fra.html

## Format des données
Les donnees sont des fichiers texte representant une grille rectangulaire avec 0 pour case vide, 1 pour case noire et la valeur de la lettre sinon \
0e0 \
10t \
010 

## Affichage des étapes
Décommenter les lignes suivantes pour obtenir l'affichage des étapes correspondantes :

Propagation : 
-L10 = actualisation grille
-L18 = afficher var fixées implicitement

BT :
-L19 = file d'attente des vars à traiter 
-L20 = var selectionnée
-L30 = mot à placer
-L33 = propagation réussie 
-L39 = propagation échouée


## Documentation
ocamldoc -html -d doc dico.mli status.mli reader.mli propagation.mli bt.mli main.ml

