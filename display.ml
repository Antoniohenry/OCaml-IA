(*module display *)

(*affiche une fenetre avec la grille d'origine*) 

(* premiere version non graphique *)
open String
open Printf

let display = fun crossword ->
        for i = 0 to (Array.length crossword) -1 do             (*parcours des lignes *)
        let line = ref "" in                                    (*texte ligne vierge *)
        for j = 0 to (Array.length crossword.(0)) -1 do                 (*parcours termes de la ligne *)
                line := String.concat !line crossword.(i).(j);          (*concatenation des termes - TODO Rajouter espace entre les termes*) 
        done;
        Printf.printf "%s\n" !line;             (* affichage ligne *)
done


