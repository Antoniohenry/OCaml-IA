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


