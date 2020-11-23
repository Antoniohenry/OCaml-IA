(* module modeler *)

(*Transforme une grille carree sauvegardee en fichier.txt en un tableau de Char puis liste de variables *)
(*Avec 0 pour case vide, 1 pour case noire et la valeur de la lettre sinon *)

(*Fonction renvoyant la largeur de la grille et une unique chaine sans separateur contenant tous les caracteres du fichier .txt*)
let model_crossword = fun file_name ->
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


(* fonction qui met la grille (chaine de caractere sans separateur) sous forme de tableau de Char*)
let get_table = fun (width, str_grid) ->
	let nb_of_lines = (String.length str_grid) / width in
	let tab_grid = Array.make_matrix nb_of_lines width '3' in (* 3 mis au pif il fallait juste un entier*)
	for line = 0 to nb_of_lines -1 do
		for col = 0 to width -1 do
			tab_grid.(line).(col) <- str_grid.[col + width * line]; (* On forme un tableau de Char *)
		done
	done;
    tab_grid


type direction = Horizontal | Vertical
type variable = { id : int; coord : int*int; lenght : int; direction : direction (*; domaine : dico; word_crossing : List of int*) } (*TODO dico et word_crossing à remplir par 2 autres fonctions ?*)

(* fonction qui annalyse la grille sous forme de tableau de Char et qui sort une liste de variables contenant les mots à trouver et les contraintes entre eux *)
let get_words = fun tab_grid ->
    let _words = ref [] in
    let _id = ref 0 in
    let _lenght = ref 0 in
    let _coord = ref (0, 0) in

    for line = 0 to (Array.length tab_grid) -1 do (*Parcours des lignes*)
        for col = 0 to (Array.length tab_grid) -1 do (*Parcours des colones*)
            match tab_grid.(line).(col) with (*Avec 0 pour case vide, 1 pour case noire et la valeur de la lettre sinon *)
                '0' -> incr _lenght
                | '1' -> if !_lenght > 0 (*cette condition permet de gerer les 1 en debut et fin de ligne*)
                                then _words := [{id = !_id; coord = !_coord; lenght = !_lenght; direction = Horizontal}] @ !_words; incr _id; (*TODO evaluer l'incrementation uniquement dans le then*)
                            _lenght := 0;
                            _coord := (line, col+1);
                | _ -> incr _lenght; (*TODO Pas de traitement particulier si on a deja une lettre -> Augmente la priorité ?*)
        done;
    if !_lenght > 0 (*cette condition permet de gerer les 1 en debut et fin de ligne*)
        then _words := [{id = !_id; coord = !_coord; lenght = !_lenght; direction = Horizontal}] @ !_words; incr _id;
    _lenght := 0;
    _coord := (line, 0);
    done;

    _lenght := 0;
    _coord := (0, 0);

    (*TODO etrange repetition des boucles d'avant -> fonction ?*)
    for col = 0 to (Array.length tab_grid) -1 do (*parcours des colones en premier cette fois*)
            for line = 0 to (Array.length tab_grid) -1 do
                match tab_grid.(line).(col) with (*Avec 0 pour case vide, 1 pour case noire et la valeur de la lettre sinon *)
                    '0' -> incr _lenght
                    | '1' -> if !_lenght > 0 (*cette condition permet de gerer les 1 en debut et fin de ligne*)
                                    then _words := [{id = !_id; coord = !_coord; lenght = !_lenght; direction = Horizontal}] @ !_words; incr _id; (*TODO evaluer l'incrementation uniquement dans le then*)
                                _lenght := 0;
                                _coord := (line+1, col);
                    | _ -> incr _lenght; (*TODO Pas de traitement particulier si on a deja une lettre -> Augmente la priorité ?*)
            done;
        if !_lenght > 0 (*cette condition permet de gerer les 1 en debut et fin de ligne*)
            then _words := [{id = !_id; coord = !_coord; lenght = !_lenght; direction = Vertical}] @ !_words; incr _id;
        _lenght := 0;
        _coord := (0, col);
        done;
    !_words

(*petite fonction d'affichage pour verifier le resultat*)
let rec print_words = fun words ->
    match words with
        [] -> ()
        | variable :: list -> Printf.printf "id : %d, long: %d\n" variable.id variable.lenght ; print_words list;;
