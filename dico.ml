(*module dictionary*)

(*generate_dictionary dico.txt *) 
(*fournit un tableau de mot trié selon la taille ou par ordre alphabetique *)

type domain = string list


let get_domain = fun file_name length ->
    let file = open_in file_name in (* ouverture du fichier *)
    let rec reader = fun words ->
        try
            let word = input_line file in
            if (String.contains word ' ') || String.length word != length  then reader words else reader (words @ [word])
        with End_of_file -> close_in file; words (* lorsqu'il n'y a plus de ligne à lire on ferme le fichier *)
    in
    reader []

let print = fun domain ->
    let rec run_througth = fun words str ->
        match words with
        [] -> str
        | head :: queue -> run_througth queue (str ^ head ^ "\n")
    in
    Printf.printf "%s" (run_througth domain "")


let get_dico = fun file_name max_length ->
    Array.init max_length (get_domain file_name)


(* TODO trouver un moyen de modifier un domain en place !! *)
let filter = fun domain car index ->
    Printf.printf "appel de filter de Dico avec lettre : %c, index : %d \n" car index;

    let is_good = fun word ->
    word.[index] = car in

    List.filter is_good domain


let next = fun domain ->
    match domain with
    [] -> raise Not_found
    | word :: domain -> (word, domain)


let is_empty = fun domain ->
    List.length domain = 0


let length = fun domain ->
    List.length domain