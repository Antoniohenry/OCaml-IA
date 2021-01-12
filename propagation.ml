(* Instanciation de var à word et propagation des contraintes *)
let rec propagation = fun status var word ->

    let saved_status = Status.copy status in (* deep_copy *)
    let id = Status.get_id var in

	Status.update status word var; (* Instanciation de var à word *)

        let rec run_neighbour = fun neighbour -> (* Propagation des contraintes aux voisins *)
	    match neighbour with
	    [] -> true 					(* On a parcouru tous les voisins en réduisant leurs domaines sans les réduire à 0 *)
        | head :: tail ->
            let domain = (Status.get_domain (Status.get_var status head)) in
            if Dico.is_empty domain then false (* Les contraintes ont vidé un domaine d'un voisin, propagation echouée *)
            else
                if (List.length domain = 1) then begin (* La propagation permet d'instancier un voisin *)
                    let (result_intern, status) = propagation status (Status.get_var status head) (List.nth domain 0) in
                    if result_intern then
                            run_neighbour tail
                    else false (* On a vidé un domaine d'un voisin de voisin.. *)
                end
                else run_neighbour tail
        in
	let result = run_neighbour (Status.get_crossed var) in

	if result then begin Status.update_queue status; (result, status) end (* Propagation reussie donc mise à jour de la file *)
	else begin (* Ce mot ne peut pas être instancié sur cette variable *)
	(* Le delete doit se faire sur une copie de la var *)
	Status.delete saved_status (Status.get_var saved_status id) word; (result, saved_status)
	end
	(* TODO Peut etre un code plus propre en renvoyant le type ('a, 'b) result d'Ocaml ? *)



