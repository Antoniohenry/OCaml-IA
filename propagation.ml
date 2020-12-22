let rec propagation = fun status var word ->

    let saved_status = Status.copy status in


	Status.update status word var;

        let rec run_neighbour = fun neighbour ->
	    match neighbour with
	    [] -> true
            | head :: tail -> 
                   let domain =  (Status.get_domain (Status.get_var status head)) in 
                   if Dico.is_empty domain then false 
                        else
                                if (List.length domain = 1) then begin
                                        (*Status.reduce_queue status head;*)
                                        let (result_intern, status) = propagation status (Status.get_var status head) (List.nth domain 0) in 
                                        if result_intern then
                                                run_neighbour tail
                                        else begin 
                                                false end 
                                end
                                else run_neighbour tail 
        in
	let result = run_neighbour (Status.get_crossed var) in

	if result then begin Status.update_queue status; (result, status) end
	else begin
	(* Le delete doit se faire sur une copie de la var *)
	Status.delete saved_status var word; (result, saved_status)
	end



