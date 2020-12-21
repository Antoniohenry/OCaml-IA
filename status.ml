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


let list_copy = fun list ->
        let rec iter = fun list new_list ->
                match list with 
                        [] -> new_list
                        | hd :: tl -> let element = hd in 
                        iter tl ( new_list @ [element])
        in
        iter list []

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




type status = {grid : grid; mutable vars : variable list; mutable queue : int list}


let get_var = fun status id ->
    List.find (fun var -> var.id = id) status.vars


let filter = fun var car index ->
    var.domain <- Dico.filter var.domain car index


let update_grid = fun grid var str ->
    let length = Bytes.index grid '\n' in
    let (line, col) = var.coord in
    match var.direction with
    (* length +1 pour passer le \n à la fin de chaque ligne *)
    Horizontal -> for index = 0 to var.length -1 do
        Bytes.set grid (line * (length +1)+ col + index) str.[index] done
    | Vertical -> for index = 0 to var.length -1 do
        Bytes.set grid  ((line + index) * (length +1) + col) str.[index] done


let update_crossing_domain = fun status var str ->
    let (line, col) = var.coord in

    let rec run = fun crossing ->
        match crossing with
        [] -> ()
        | id :: queue ->
            begin
            let var_crossed = get_var status id in
            Printf.printf "update domain : "; print_var var_crossed;
            let (l, c) = var_crossed.coord in
            begin match var.direction with
            Vertical ->
                let var_crossed_index = col - c in
                let letter = str.[l - line] in
                filter var_crossed letter var_crossed_index;
                run queue

            | Horizontal ->
                let var_crossed_index = line - l in
                let letter = str.[c - col] in
                filter var_crossed letter var_crossed_index;
                run queue
            end
        end
        in
    run var.crossing


let update = fun status str var ->
    Printf.printf "set current variable : "; print_var var; Printf.printf "with : %s\n" str;
    var.domain <- [str];

    update_grid status.grid var str;

    (* vide les domaines des mots croises *)
    update_crossing_domain status var str


(* fonction d'affichage de la grille *)
let print_grid = fun status ->
    Printf.printf "%s \n" (Bytes.to_string status.grid)



(* petite fonction d'affichage d'une variable list pour verifier le resultat *)
let rec print_vars = fun vars ->
    match vars with
    [] -> ()
    | var :: list -> begin print_var var; print_vars list end


let get_domain = fun var ->
    var.domain


let update_queue = fun status ->

    (* permet d'enlever les variables déjà instanciées (notamment celle qu'on vient juste d'intancier lors de la propa) *)
    let vars = List.filter (fun var -> (List.length var.domain) > 1) status.vars in

    let comp = fun var1 var2 ->
        compare (Dico.length (get_domain var1)) (Dico.length (get_domain var2))
        in
    let sorted = List.sort comp vars in
    status.queue <- List.map (fun var -> var.id) sorted

let get_queue = fun status ->
    status.queue


let is_queue_empty = fun status ->
    List.length (get_queue status) = 0

let select_var = fun status ->
    begin if is_queue_empty status then update_queue status end; (* Pour regler un pb au premier appel *)
    Printf.printf "length queue : %d\n" (List.length status.queue);
    let id = List.hd status.queue in
    let var = List.find (fun var -> var.id = id) status.vars in
    let length = Dico.length (get_domain var) in
    if length = 0 then (false, var) else (true, var)


let get_neighbour = fun status id ->
    let var = get_var status id in
    var.crossing


(* 1er element de la file *)
let get_element = fun status ->
    List.hd status.queue


let set_domain = fun status var domain ->
    (List.nth status.vars var.id).domain <- domain

let get_crossed = fun var ->
    var.crossing

let set_var = fun id coord length direction domain ->
    { id = id; coord = coord; length = length; direction = direction; domain = domain; crossing = []}

let copy_queue = fun queue -> list_copy queue

let copy_vars = fun variables ->
        let rec iter = fun vars new_vars ->
                match vars with 
                        [] -> new_vars
                        | hd::tl -> let var = {id = hd.id ; coord = hd.coord ; length = hd.length ; direction = hd.direction ; domain = (list_copy hd.domain) ; crossing = (list_copy hd.crossing) } in
                iter tl (new_vars @ [var])
        in
        iter variables []

let copy = fun status -> 
        let grid = Bytes.copy status.grid in
        let vars = copy_vars status.vars in
        let queue = copy_queue status.queue in
        {grid = grid; vars = vars; queue =  queue }

let print_queue = fun queue ->
        let rec iter = fun str queue ->
                match queue with 
                [] -> str
                | hd::tl -> iter (str ^ (Int.to_string hd) ^ " ") tl
        in
        Printf.printf "%s" (iter "" queue)
