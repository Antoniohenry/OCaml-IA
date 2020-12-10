type grid = bytes

type direction = Horizontal | Vertical

type domain = string list

type variable = {
    id : int;
    coord : int*int; (* ligne, colonne *)
    length : int;
    direction : direction;
    mutable domain : domain;
    mutable crossing : int list }


type status = {grid : grid; mutable vars : variable list; mutable queue : int list}


let update = fun status str var ->
    let id = var.id in
    let is_good = fun id var ->
        var.id = id in
    let var = List.find (is_good id) status.vars in
    var.domain <- [str];

    let length = Bytes.index status.grid '\n' in

    let (line, col) = var.coord in
    match var.direction with
    Horizontal -> for index = 0 to var.length do
        Bytes.set status.grid (line * length + col + index) str.[index] done
    | Vertical -> for index = 0 to var.length do
        Bytes.set status.grid  ((line + index) * length + col) str.[index] done


(* fonction d'affichage de la grille *)
let print_grid = fun grid ->
    Printf.printf "%s" grid


(* affichage d'une variable *)
let print_var = fun var ->
    let (x,y) = var.coord in
    let dir = if var.direction = Horizontal then "horizontal" else "vertical" in

    (* parcourt la liste d'entier crossed et renvoie la chaine de caractere coreespondante *)
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


let get_domain = fun var ->
    var.domain


let update_queue = fun status ->
    let vars = status.vars in
    let comp = fun var1 var2 ->
        compare (Dico.length (get_domain var1)) (Dico.length (get_domain var2))
        in
    let sorted = List.sort comp vars in
    status.queue <- List.map (fun var -> var.id) sorted


let select_var = fun status ->
    let id = List.hd status.queue in
    let var = List.find (fun var -> var.id = id) status.vars in
    let length = Dico.length (get_domain var) in
    if length = 0 then (false, var) else (true, var)


let get_var = fun status id ->
    List.find (fun var -> var.id = id) status.vars


let get_neighbour = fun status id ->
    let var = get_var status id in
    var.crossing

let get_queue = fun status ->
    status.queue

let is_queue_empty = fun status ->
    List.length (get_queue status) = 0

(* 1er element de la file *)
let get_element = fun status ->
    List.hd status.queue


let set_domain = fun status var domain ->
    (List.nth status.vars var.id).domain <- domain

let get_crossed = fun var ->
    var.crossing

let set_var = fun id coord length direction domain ->
    { id = id; coord = coord; length = length; direction = direction; domain = domain; crossing = []}