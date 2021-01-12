
type domain = string list

(* TODO on ouvre et ferme plusieurs fois le fichier inutilement *)
let get_domain = fun file_name length -> (* length : longeur de la variable *)
    let file = open_in file_name in (* ouverture du fichier *)
    let rec reader = fun words ->
        try
            let word = input_line file in
            (* on enleve les mots contenant un espace (expression) et les mots qui n'ont pas la bonne longueur *)
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


(* Renvoie le tableau des domaines *)
let get_dico = fun file_name max_length -> (* max_length : taille maximale de la grille *)
    Array.init max_length (get_domain file_name)


(* renvoie le nouveau domain filtré des mots n'ayant pas car en position index (commence à 0) *)
let filter = fun domain car index ->
    let is_good = fun word ->
    word.[index] = car in
    List.filter is_good domain


let is_empty = fun domain ->
    List.length domain = 0


let length = fun domain ->
    List.length domain
