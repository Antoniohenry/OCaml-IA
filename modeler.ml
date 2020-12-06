(* module modeler *)

(*Transforme une grille sauvegardée en fichier.txt en un tableau de tableau / modele de mots croisees  *)
(*Les donnees sont alors la valeur de la case avec 0 pour case vide, 1 pour case noire et la valeur de la lettre sinon *)

open Printf

let model_crossword = fun file_name ->
    (*try*)
	    let file = open_in file_name in (* ouverture du fichier *)
	    let line = input_line file in (* recuperation de la 1ere ligne *)
	    let width = String.length line in
	    let rec reader = fun str_acc -> (* fonction qui accumule toutes les lignes sous forme de chaine de caractere*)
	       	try
		        let line = input_line file in
		        reader (str_acc ^ line)
		    with End_of_file -> close_in file; str_acc; in (* lorsqu'il n'y a plus de ligne à lire on ferme le fichier *)
	    let str_grid = reader line in
	    (width, str_grid)
	(*with _ -> Printf.printf "impossible d'ouvrir %s\n" file_name*)


(* fonction qui met la grile sous forme de tableau*)
let get_table = fun (width, str_grid) ->
	let nb_of_lines = (String.length str_grid) / width in
	let tab_grid = Array.make_matrix nb_of_lines width '3' in (* 3 mis au pif il fallait juste un entier*)
	for line = 0 to nb_of_lines -1 do
		for col = 0 to width -1 do
			tab_grid.(line).(col) <- str_grid.[col + width * line]; (* On forme un tableau de case *)
		done
	done;
    tab_grid
(* fonction qui annalyse la grille sous forme de tableau et qui sort une structure (grid?) contenant les mots à trouver et les contraintes entre eux *)

(* TODO Mettre les paramètres en interne des fonctions des que possible pour alleger le programme *)
(* TODO rendre les données mutables *)
