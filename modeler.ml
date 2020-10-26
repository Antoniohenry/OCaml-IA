(* module modeler *)

(*Transforme une grille sauvegardée en fichier.txt en un tableau de tableau / modele de mots croisees  *)
(*Les donnees sont alors la valeur de la case avec 0 pour case vide, 1 pour case noire et la valeur de la lettre sinon *)

let model_crossword = fun file_name -> try 
        let grid = open_in file_name in                 (*ouverture du fichier avec gestion exceptions/erreurs *)
        let dimensions = Scanf.sscanf (input_line grid) "%d %d" (fun n m -> [|n;m|]) in         (*lecture de la premiere ligne avec les dimensions du tableau TODO possible de le déduire du reste *)
        let crossword = Array.make dimensions.(0) (Array.make dimensions.(1) 0) in              (*creation tableau de tableau de 0 *)
        let fill_element = fun grid_line nb_case element ->             (*fct interne remplissage des cases de chaque ligne *)
                grid_line.[nb_case] in
        let fill_line = fun line ->                                     (* fct interne remplissage des lignes de la grille modelisee *)
                let grid_line = input_line grid in
                Array.mapi (fill_element grid_line) line in             (* application à chaque element (de la ligne) de la fct fill element *)
        Array.map fill_line crossword                                   (* application à chaque ligne de la fct fill_line *)
with exc -> Printf.printf "%s ne s'ouvre pas en lecture\n" file_name; raise exc
