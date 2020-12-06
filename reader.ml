(*Transforme une grille rectangulaire sauvegardee en fichier.txt en liste de variables *)
(*Avec 0 pour case vide, 1 pour case noire et la valeur de la lettre sinon *)

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
    (width, !height, grid)


type direction = Horizontal | Vertical

type variable = {
    id : int;
    coord : int*int; (* ligne, colonne *)
    length : int;
    direction : direction;
    (* muatble domaine : dico *)
    mutable crossing : int list }


(* petite fonction d'affichage pour verifier le resultat *)
let rec print_words = fun words ->
    match words with
    [] -> ()
    | variable :: list -> let (x,y) = variable.coord in
        let dir = if variable.direction = Horizontal then "horizontal" else "vertical" in

        (* parcourt la liste d'entier crossed et renvoie la chaine de caractere coreespondante *)
        let crossed = variable.crossing in
        let rec run_crossed = fun str crossed index ->
            match crossed with
            [] -> str
            | head :: queue ->
                run_crossed (str ^ (Int.to_string head) ^ ", ") queue (index +1) in
        let str = run_crossed "" crossed 0 in

        Printf.printf "id : %d, long: %d, (%d, %d), %s, crossed : %s\n" variable.id variable.length x y dir str;
        print_words list


(* ajoute un word a words *)
let add = fun words _coord _length _direction ->
    if _length <= 1 then words (* permet d'enlever les mots d'une lettre et les mots sans lettre (en fin de ligne) *)
    else let _id = List.length words in
    words @ [{ id = _id; coord = _coord; length = _length; direction = _direction; crossing = []}]


(* transfomre un Dtring en Char list *)
let explode = fun s ->
    List.init (String.length s) (String.get s)


(*
    ajoute a la list _words les mots contenus dans str (representant une ligne ou une colonne)
    direction = Horizontal | Vertical
    position est l'index de la ligne ou de la colonne dans la grille
*)
let get_words_from_string = fun str _words direction position ->

    let str_length = String.length str and words = ref _words in

    let rec run_throught = fun str length ->
        let index = str_length - (List.length str) - length in (* index de la premiere lettre du mot  *)
        match str with
        [] ->
            let coord = if direction = Horizontal then (position, index)
                        else (index, position) in
            words := add !words coord length direction
        | head :: queue ->
            begin match head with
            '1' ->
                let coord = if direction = Horizontal then (position, index)
                            else (index, position) in
                words := add !words coord length direction;
                run_throught queue 0
            | '0' ->
                run_throught queue (length +1)
            | _ ->
                run_throught queue (length +1)
            end
    in
    run_throught (explode str) 0;
    !words


(* renvoie un booleen *)
let is_crossing = fun word1 word2 ->
    if word1.direction = word2.direction then false
    else let (vword, hword) = if word1.direction = Vertical then (word1, word2)
                              else (word2, word1) in

         let (h_line, hx) = hword.coord and (vy, v_col) = vword.coord in

         (hx <= v_col) && (v_col <= (hx + hword.length -1)) && (vy <= h_line) && (h_line <= (vy + vword.length -1))


(* complete les listes crossing de word1 et word2 si necessaire *)
let check_crossing = fun word1 word2 ->
    if is_crossing word1 word2 then begin
        word1.crossing <- List.append word1.crossing [word2.id];
        word2.crossing <- List.append word2.crossing [word1.id]
        end


(* remplie toutes les listes crossing pour chaque word dans words, renvoie unit *)
let rec get_crossed = fun words ->
    match words with
    [] -> ()
    | head :: queue ->
        List.iter (check_crossing head) queue;
        get_crossed queue


(* parcourt les lignes et colonnes de grid et renvoie la listes des words *)
let get_words = fun width height grid ->

    let words = ref [] in

    let i = ref 0 in
    while !i < height do
        let line = String.sub grid (!i*(width +1)) width in
        words := get_words_from_string line !words Horizontal !i;
        incr i
        done;

    let j = ref 0 in
    while !j < width do
        let column = Bytes.create height in
        for index = 0 to Bytes.length column -1 do
            Bytes.set column index (String.get grid (!j + index * (width + 1) ) )
            done;
        words := get_words_from_string (Bytes.to_string column) !words Vertical !j;
        incr j;
        done;
    get_crossed !words;
    !words


(* fonction d'affichage de la grille *)
let print_grid = fun grid ->
    Printf.printf "%s" grid


(* transforme une grille rectangulaire sauvegardee en fichier.txt en liste de variables *)
let get_words_from_txt = fun fic_name ->
    let (width, heigth, grid) = get_grid fic_name in
    get_words width heigth grid