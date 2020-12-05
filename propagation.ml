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


