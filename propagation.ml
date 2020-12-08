let propagation = fun status index word ->
	status.grid = grid_update status.grid status.variables.(index) word;
	status.variables = variables_update status.grid status.variables index;
	let result = ref true in 
	for id in status.variables.(index).ncl do
		if (List.length status.variables.(id).domain) = 0 then result := false
	done
	if not result then (false, status)
	else status.rvi = remain_var_update status.rvi status.variables.(index);
	(true, status);;

let grid_update = fun grid variable word ->
	let direction = variable.direction in
	let coord = variable.start_position in
        match direction with
                Horizontal -> for j=0 to variable.length do
                                grid.(coord.(0)).(coord.(1)+j) = word.[j];
                        done
                            
                | Vertical -> for j=0 to variable.length do 
                                grid.(coord.(0)+j).(coord.(1)) = word.[j];
                        done;
                        grid (*à enlever si mutable *)

let variables_update = fun grid variables index_variable ->
                
        let update_domain = fun id_ngh ->
                begin match variables.(id_ngh).direction with (* voisin horizontal, variable verticale *)
                        Horizontal -> let crossing_column = variables.(index_variable).(1) in (* index colonne*)
                                let crossing_line = variables.(id_ngh).(0)  in (*index ligne *)
                                let char_id = crossing_column - variables.(id_ngh).(1) in (* index du char dans le voisin *)
                        |Vertical -> let crossing_column = variables.(id_ngh).(1) in
                                let crossing_line = variables.(index_variable).(0) in
                                let char_id = crossing_line - variables.(id_ngh).(0) in
                end; (* probleme à confirmer ? *)
                let character = grid.(crossing_line).(crossing_column) in
                Dico.filter variables.(id_ngh).domain character char_id;
                       (*enlever chaque mot du domaine de id_ngh qui n'a pas char a la position char_id*)
        in
        
        for id_ngh=0 to (List.length variables.(index_variable).ncl) do (* variables supposée être une liste *)
                variables.(id_ngh).domain = update_domain grid variables.(id); (* en place avec mutable *)
        done
        (* Possibilité d'avoir une grille de n*n avec dans chaque case : (id de la variable horiz, id de la variable verti, coord_x, coord_y) ?? *)

let remain_var_update rvi variable ->
        rvi = List.tl rvi;
                match rvi with
                [] -> rvi
                | _-> rvi; (* trier la liste selon variable.ngh*)



(* fonctions auxiliaires *)
let is_rvi_empty = fun  status -> ((Array.length status.rvi) = 0);;
let get_index_rvi = fun rvi -> rvi.(0);;
let get_rvi = fun status -> status.rvi;;  
let get_var = fun status index -> status.variables.(index);;
let get_domain = fun var -> var.domain;;
let domain_reduce = fun domain -> 
        Dico.dom_reduce domain;; (* liste -> tl ou array mais dans le module *)
let is_domain_empty = fun var -> Dico.is_empty var.domain;;


