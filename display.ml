(*module display *)

(*affiche une fenetre avec la grille d'origine*) 

(* premiere version non graphique *)
open String
open Printf

let display = fun crossword ->
        for i = 0 to (Array.length crossword) -1 do             (*parcours des lignes *)
                let line = ref "" in
                for j = 0 to (Array.length crossword.(0) -1) do                         (*parcours des termes de la ligne *)
                        line := String.concat " " [!line; Char.escaped crossword.(i).(j) ];          (*concatenation des termes de la liste [] avec le séparateur espace*) 
                done;                                                                                   (*On a crossword un tableau de Char donc conversion en String pour concatener*)
                Printf.printf "%s\n" !line;             (* affichage ligne *)
        done


