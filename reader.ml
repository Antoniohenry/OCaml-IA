(*Transforme une grille rectangulaire sauvegardee en fichier.txt en liste de variables *)
(*Avec 0 pour case vide, 1 pour case noire et la valeur de la lettre sinon *)

open Status

type status = Status.status
type variable = Status.variable

(* renvoie la (largeur, la hauteur, grille) du fichier .txt*)
let get_grid = fun file_name ->
    let file = open_in file_name in (* ouverture du fichier *)
    let line = input_line file in (* recuperation de la 1ere ligne *)
    let width = String.length line in
    let height = ref 1 in (*1 et pas 0 car reader est appele avec une ligne deja lue*)
    let rec reader = fun str_acc -> (* fonction qui accumule toutes les lignes sous forme de chaine de caractere*)
        try
            let line = input_line file in
            incr height;
            reader (str_acc ^ line ^ "\n");
        with End_of_file -> close_in file; str_acc; in (* lorsqu'il n'y a plus de ligne à lire on ferme le fichier *)
    let grid = reader (line ^ "\n") in
    (width, !height, Bytes.of_string grid)


(* ajoute une variable a vars, renvoie vars *)
let add = fun vars coord length direction domain constraints ->
    if length <= 1 then vars (* permet d'enlever les mots d'une lettre et les mots sans lettre (en fin de ligne) *)
    else let id = List.length vars in
    let rec run_constraints = fun constraints domain ->
        match constraints with
        [] -> domain
        | (letter, index) :: queue -> run_constraints queue (Dico.filter domain letter index)
        in
    let _domain = run_constraints constraints domain in
    vars @ [Status.set_var id coord length direction _domain]


(* transforme un String en Char list *)
let explode = fun s ->
    List.init (String.length s) (String.get s)


(*
    ajoute a la list _vars les variables contenues dans str (representant une ligne ou une colonne)
    direction = Horizontal | Vertical
    position est l'index de la ligne ou de la colonne dans la grille
    renvoie vars
*)
let get_vars_from_string = fun str _vars direction position dico ->

    let str_length = String.length str and vars = ref _vars in

    let rec run_throught = fun str length constraints ->
        let index = str_length - (List.length str) - length in (* index de la premiere lettre du mot  *)
        match str with
        [] ->
            let coord = if direction = Status.Horizontal then (position, index)
                        else (index, position) in
            vars := add !vars coord length direction dico.(length) constraints
        | head :: queue ->
            begin match head with
            '1' ->
                let coord = if direction = Status.Horizontal then (position, index)
                            else (index, position) in
                vars := add !vars coord length direction dico.(length) constraints;
                run_throught queue 0 constraints
            | '0' ->
                run_throught queue (length +1) constraints
            | letter ->
                run_throught queue (length +1) (constraints @ [(letter, length)])
            end
    in
    run_throught (explode str) 0 [];
    !vars


(* renvoie un booleen *)
let is_crossing = fun word1 word2 ->
    if word1.direction = word2.direction then false
    else let (vword, hword) = if word1.direction = Status.Vertical then (word1, word2)
                              else (word2, word1) in

        let (h_line, hx) = hword.coord and (vy, v_col) = vword.coord in
        (* -1 car les lignes et colonnes commencent à 0 *)
        (hx <= v_col) && (v_col <= (hx + hword.length -1)) && (vy <= h_line) && (h_line <= (vy + vword.length -1))


(* complete les listes crossing de var1 et var2 si necessaire, en place *)
let check_crossing = fun var1 var2 ->
    if is_crossing var1 var2 then begin
        var1.crossing <- List.append var1.crossing [var2.id];
        var2.crossing <- List.append var2.crossing [var1.id]
        end


(* remplie toutes les listes crossing pour chaque var dans vars, en place *)
let rec get_crossed = fun vars ->
    match vars with
    [] -> ()
    | head :: queue ->
        List.iter (check_crossing head) queue;
        get_crossed queue


(* parcourt les lignes et colonnes de la grille et renvoie une varriable list *)
let get_vars = fun width height grid dico ->
    let vars = ref [] in
    for i=0 to height-1 do
	let line = String.sub grid (i*(width +1)) width in (* +1 car il y a des \n à la fin des lignes *)
        vars := get_vars_from_string line !vars Status.Horizontal i dico;
    done;

    for j=0 to height-1 do
        let column = Bytes.create height in
        for index = 0 to Bytes.length column -1 do
            Bytes.set column index (String.get grid (j + index * (width + 1) ) ) (* +1 car il y a des \n à la fin des lignes *)
            done;
        vars := get_vars_from_string (Bytes.to_string column) !vars Status.Vertical j dico;
    done;
    get_crossed !vars;
    !vars


(* transforme une grille rectangulaire sauvegardee en fichier.txt en un status *)
let read = fun fic_name dico_name ->
    let (width, heigth, grid) = get_grid fic_name in
    let dico = Dico.get_dico dico_name ((max width heigth) +1) in
    let vars = get_vars width heigth (Bytes.to_string grid) dico in
    let status = {grid = grid; vars = vars; queue = []} in
    Status.update_queue status;
    status
