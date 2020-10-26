(*module display *)

(*affiche une fenetre avec la grille d'origine*) 

(* premiere version non graphique *)
open String
open Printf

let display = fun crossword ->
        for i = 0 to (Array.length crossword) -1 do
let line = ref "" in
for j = 0 to (Array.length crossword.(0)) -1 do
line := String.concat !line crossword.(i).(j);
done;
Printf.printf "%s\n" !line; 
done


