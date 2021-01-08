(* sauvegarde le statut *)
let save = fun status ->
        Status.copy status

exception Back of bool (* Permet de savoir si on remonte dans l'arbre *)


let rec bt = fun status ->

    Printf.printf " \n \nnouveau bt \n";
    Status.print_grid status;

    let status_saved = save status in (* sauvegarde du statut *)
    (*Printf.printf "grille sauvegardée: \n"; Status.print_grid status_saved; *)

    if Status.is_queue_empty status then begin (Status.print_grid status); failwith "Solution" end
    else
        let var = Status.select_var status in
        Printf.printf "\nvariable selectionnée : "; Status.print_var var;

        (* On parcourt le domain de la variable sélectionnée *)
        let rec run = fun status domain ->
            match domain with
            (* On a parcouru tout le domaine sans trouver la solution *)
            [] -> Printf.printf "Backtrack ! \n"; raise (Back true) (* On doit remonter dans l'arbre *)

            (* On teste la propagation sur chaque mot du domaine *)
            | word :: remain_domain ->
                Printf.printf "mot à placer %s :" word;
                let (propa_result, status_apres_propa) = Propagation.propagation status var word in
                if propa_result then begin
                    Printf.printf "propagation réussie \n";

                    (* Dans tous les cas on evalue 'bt status_apres_propa' ie on plonge dans l'arbre *)
                    (* si true alors c'est qu'apres avoir plongé dans l'arbre on veut backtracker sur ce noeud -> il suffit d'abandonner le mot fixé précedement pour passer au mot suivant *)
                    if bt status_apres_propa then run status_saved remain_domain end (* c'est cette ligne qui gere la pile des appels recursifs *)
                else begin
                    Printf.printf "propa echouée \n";
                    Status.delete status_apres_propa var word;
                    run status_apres_propa remain_domain end
            in
        try
        run status var.domain; false (* Si on arrive à ce false alors la grille n'a pas de solution (TODO à vérifier!!) *)

        (* Si on parcourt tout le domain sans propagation reussie alors il faut backtracker ie bt doit renvoyer true *)
        with Back bl -> bl
