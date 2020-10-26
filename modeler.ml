(* module modeler *)

(*Transforme une grille sauvegardée en fichier.txt en un tableau de tableau *)
(*Les donnees sont alors la valeur de la case avec 0 pour case vide, 1 pour case noire et la valeur de la lettre sinon *)

let model_crossword = fun file_name ->
try 
let grid = open_in file_name in
let dimensions = Scanf.sscanf (input_line grid) "%d %d" (fun n m -> [|n;m|]) in 
let crossword = Array.make dimensions.(0) (Array.make dimensions.(1) 0) in
let fill_element = fun grid_line nb_case element ->
grid_line.[nb_case] in
let fill_line = fun line -> 
let grid_line = input_line grid in
Array.mapi (fill_element grid_line) line in
Array.map fill_line crossword
with exc -> Printf.printf "%s ne s'ouvre pas en lecture\n" file_name; raise exc
