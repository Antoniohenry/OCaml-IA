(* module modeler *)

(*Transforme une grille sauvegardée en fichier.txt en un tableau de tableau / modele de mots croisees  *)
(*Les donnees sont alors la valeur de la case avec 0 pour case vide, 1 pour case noire et la valeur de la lettre sinon *)

open Printf

type case =
	Integer of int
	| Letter of char

let model_crossword = fun file_name -> try
	let file = open_in file_name in (* ouverture du fichier *)
	let line = input_line file in (* recuperation de la 1ere ligne *)
	let width = String.length line in
	let rec reader = fun str_acc -> (* fonction qui accumule toutes les lignes sous forme de chaine de caractere*)
		try
		let line = input_line file in
		reader (str_acc ^ line)
		with End_of_file -> str_acc; close_in file; (* lorsqu'il n'y a plus delgne à lire on ferme le fichier *)
	let str_grid = reader line in
	Printf.printf "%s\n" str_grid  (* TODO renvoyer largeur et chaine + TODO erreur syntaxe *)
	with _ -> Printf.printf "impossible d'ouvrir %s\n" file_name;


(* fonction qui met la grile sous forme de tableau*)
let get_table = fun width str_grid -> (* TODO à tester !*)
	let nb_of_lines = (String.length str_grid) / width in
	let tab_grid = Array.make_matrix nb_of_lines width int_of_sting("-1") in (* -1 mis au pif il fallait juste un entier*)
	for line = 0 to nb_of_lines do
		for col = 0 to width do
			let get_elt = fun caracter ->
				match caracter with
					Integer "1" -> 1
					| Integer "0" -> 0
					| Letter _ -> _
			tab_grid.(line).(col) = get_elt str_grid[line + width * col] (* On forme un tableau de case *)
		done;
	done

(* fonction qui annalyse la grille sous forme de tableau et qui sort une structure (grid ?)
contenant les mots à trouver et les contraintes entre eux *)