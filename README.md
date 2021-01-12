# OCaml-IA

## Lancement du programme 
    
    ./main [file] [option] 
    file : exemple.txt par defaut
    Option 
    all : recherche et affiche toutes les solutions 
    print_bt : affiche la grille à chaque backtrack 
    print_propa : affiche la grille à chaque propagation reussie
    Toutes les options sont desactivées par défaut !

###Exemple d'utilisation :

    ./main grille.txt all
    ./main all print_bt print_propa


## Format des données
Les donnees sont des fichiers texte representant une grille rectangulaire avec 0 pour case vide, 1 pour case noire et la valeur de la lettre sinon \
0e0 \
10t \
010

## How To Makefile

https://caml.inria.fr/pub/old_caml_site/FAQ/Makefile_ocaml-fra.html


## Documentation
    
    ocamldoc -html -d doc dico.mli status.mli reader.mli propagation.mli bt.mli main.ml

