let propagation = fun status var word ->
	Status.update status word var;

	let rec run_neighbour = fun neighbour ->
	    match neighbour with
	    [] -> true
	    | head :: queue -> if Dico.is_empty (Status.get_domain (Status.get_var status head)) then false else run_neighbour queue
	    in
	let result = run_neighbour (Status.get_crossed var) in

	begin if result then Status.update_queue status end;
	(result, status)


