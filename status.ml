type grid = bytes

type direction = Horizontal | Vertical

type domain = string list

type variable = {
    id : int;
    coord : int*int; (* ligne, colonne commence à 0 !! *)
    length : int;
    direction : direction;
    mutable domain : domain;
    mutable crossing : int list }

type status = {grid : grid; mutable vars : variable list; mutable queue : int list (* mutable : int list *)}

(* Fonction d'affichage *)

(* fonction d'affichage de la grille *)
let print_grid = fun status ->
    Printf.printf "%s" (Bytes.to_string status.grid)


(* affichage d'une variable *)
let print_var = fun var ->
    let (x,y) = var.coord in
    let dir = if var.direction = Horizontal then "horizontal" else "vertical" in

    (* parcourt la liste d'entier crossed et renvoie la chaine de caractere correspondante *)
    let crossed = var.crossing in
    let rec run_crossed = fun str crossed index ->
        match crossed with
        [] -> str
        | head :: queue ->
            run_crossed (str ^ (Int.to_string head) ^ ", ") queue (index +1) in
    let str = run_crossed "" crossed 0 in
    let size = List.length var.domain in
    Printf.printf "id : %d, long: %d, (%d, %d), %s, domain size: %d, crossed : %s\n" var.id var.length x y dir size str


(* petite fonction d'affichage d'une variable list pour verifier le resultat *)
let rec print_vars = fun vars ->
    match vars with
    [] -> ()
    | var :: list -> begin print_var var; print_vars list end


let print_queue = fun queue ->
        let rec iter = fun str queue ->
                match queue with
                [] -> str
                | hd::tl -> iter (str ^ (Int.to_string hd) ^ " ") tl
        in Printf.printf "%s" (iter "" queue)


(* Fonction permettant de copier un Status *)

let list_copy = fun list ->
    let array = Array.of_list list in
    let new_array = Array.copy array in
    Array.to_list new_array

let copy_queue = fun queue -> list_copy queue

let copy_var = fun var ->
    {id = var.id ; coord = var.coord ; length = var.length ; direction = var.direction ; domain = (list_copy var.domain) ; crossing = (list_copy var.crossing) }

let copy_vars = fun variables ->
        let rec iter = fun vars new_vars ->
                match vars with
                        [] -> new_vars
                        | hd::tl -> let var = copy_var hd in
                iter tl (new_vars @ [var])
        in
        iter variables []


let copy = fun status ->
        let grid = Bytes.copy status.grid in
        let vars = copy_vars status.vars in
        let queue = copy_queue status.queue in
        {grid = grid; vars = vars; queue =  queue }


(* Getter et Setter *)

let get_var = fun status id ->
    List.find (fun var -> var.id = id) status.vars

let get_domain = fun var ->
    var.domain

let get_crossed = fun var ->
    var.crossing

let get_id = fun var ->
    var.id

(* renvoie la premiere variable de la file *)
let select_var = fun status ->
    let id = List.hd status.queue in
    let var = List.find (fun var -> var.id = id) status.vars in
    var

let set_var = fun id coord length direction domain ->
    { id = id; coord = coord; length = length; direction = direction; domain = domain; crossing = []}

(* supprime word du domaine de var dans status en place *)
let delete = fun status var word ->
    var.domain <- List.filter (fun w -> not (String.equal w word)) var.domain;
    status.vars <- List.filter (fun variable -> variable.id != var.id) status.vars;
    status.vars <- status.vars @ [var]


(* Fonction de mise à jour du Status utilisées notamment lors de la propagation *)

(* uniquement la mise à jour de l'affichage *)
let update_grid = fun grid var str ->
    let length = Bytes.index grid '\n' in
    let (line, col) = var.coord in
    match var.direction with
    (* length +1 pour passer le \n à la fin de chaque ligne *)
    Horizontal -> for index = 0 to var.length -1 do
        Bytes.set grid (line * (length +1)+ col + index) str.[index] done
    | Vertical -> for index = 0 to var.length -1 do
        Bytes.set grid  ((line + index) * (length +1) + col) str.[index] done

(* Met à jour les domaines des voisins *)
let update_crossing_domain = fun status var str ->
    let filter = fun var car index ->
        var.domain <- Dico.filter var.domain car index
        in
    let (line, col) = var.coord in
    let rec run = fun crossing ->
        match crossing with
        [] -> ()
        | id :: queue ->
            begin
            let var_crossed = get_var status id in
            let (l, c) = var_crossed.coord in (* coordonnee de la première lettre du voisin *)
            begin match var.direction with
            Vertical ->
                let var_crossed_index = col - c in (* position de la lettre ou les mots se croisent *)
                let letter = str.[l - line] in
                filter var_crossed letter var_crossed_index;
                run queue
            | Horizontal ->
                let var_crossed_index = line - l in (* position de la lettre ou les mots se croisent *)
                let letter = str.[c - col] in
                filter var_crossed letter var_crossed_index;
                run queue
            end
        end
        in run var.crossing


(* met à jour la liste des voisins (var disparait des crossing) *)
let update_crossing = fun status var ->
    let rec run = fun neighbours_id ->
        match neighbours_id with
        [] -> ()
        | id :: tl ->
                (get_var status id).crossing <- List.filter (fun id -> (id != var.id)) (get_var status id).crossing;
                run tl
        in run var.crossing



let update = fun status str var ->
    var.domain <- [str];
    update_grid status.grid var str;
    (* vide les domaines des mots croises *)
    update_crossing_domain status var str;
    (* on supprime var des listes des voisins *)
    update_crossing status var


let update_queue = fun status ->
    (* permet d'enlever les variables déjà instanciées (notamment celle qu'on vient juste d'intancier lors de la propa) *)
    let vars = List.filter (fun var -> (List.length var.domain > 1))  status.vars in

    (* fonction de tri sur la longueur du domaine *)
    let comp = fun var1 var2 ->
        compare (Dico.length (get_domain var1)) (Dico.length (get_domain var2))
        in
    let sorted = List.sort comp vars in
    status.queue <- List.map (fun var -> var.id) sorted

let is_queue_empty = fun status ->
    List.length (status.queue) = 0
