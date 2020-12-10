type grid = string



type direction = Horizontal | Vertical

type variable = {
    id : int;
    coord : int*int; (* ligne, colonne *)
    length : int;
    direction : direction;
    domain : domain;
    mutable crossing : int list }


type state = {grid : grid; vars : variable list; queue : int list}


let update = fun grid str id variables ->
    let is_good = fun id var ->
        var.id = id in
    let var = List.find (is_good id) variables in
    print_var var
    (* TODO à finir maj grid et variable list *)


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


(*
let grid_update = fun grid variable word ->
	let direction = variable.direction in
	let coord = variable.start_position in
        match direction with
                H -> for j=0 to variable.length do
                                grid.(coord.(0)).(coord.(1)+j) = word.[j];
                        done

                | V -> for j=0 to variable.length do
                                grid.(coord.(0)+j).(coord.(1)) = word.[j];
                        done;
                        grid

let variables_update = fun grid variables index_variable ->

        let update_domain = fun grid id_ngh index_variable ->
                match variables.(id_ngh).direction with
                        H -> let char_id = variables.(index_variable).(1) - variables.(id_ngh).(1) in
                                let char = grid.(variables.(id_ngh).(0)).(variables.(index_variable).(1)) in
                                (*enlever chaque mot du domaine de id_ngh qui n'a pas char a la position char_id*)
        in
        for id_ngh=0 to (List.length variables.(index_variable).ncl) do
                variables.(id_ngh).domain = update_domain grid variables.(id);
        done
        (* TODO à finir *)
        (* Possibilité d'avoir une grille de n*n avec dans chaque case : (id de la variable horiz, id de la variable verti, coord_x, coord_y) ?? *)
*)

let update_rvi = fun vars -> queue


let select_var = fun state
 -> (bool, var)


let get_var = fun id -> var

let get_neighbour = fun id -> int list

let get_queue = fun state -> int list

let is_queue_empty = fun state -> bool

let get_element = fun state -> id (* 1er element de la file *)

let get_domain = fun var -> var.domain