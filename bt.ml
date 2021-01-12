(* sauvegarde le statut *)
let save = fun status ->
        Status.copy status

exception Back of bool (* Permet de savoir si on remonte dans l'arbre *)

(* Utilisé dans les affichages *)
type count = {bt : int; propa : int ; sol : int}
let count = ref {bt = 0; propa = 0; sol = 0}

let print_count = fun count all ->
    let bt = count.bt in
    let propa = count.propa in
    let sol = count.sol in
    Printf.printf "\nNombre de backtrack : %d \nNombre de propagations réussies : %d\n" bt propa; 
    if all then Printf.printf "Numéro de la solution : %d \n" sol


(* Appelée à chaque backtrack *)
let each_bt = fun status print ->
    if print then begin
    Printf.printf "\nBacktrack ! \n";
    Status.print_grid status end;
    let bt = !count.bt in
    let propa = !count.propa in
    let sol = !count.sol in
    count := {bt = bt + 1; propa = propa; sol = sol}

(* Appelée à chaque propagation reussie *)
let each_propa = fun status print ->
    if print then begin
    Printf.printf "\nPropagation reussie ! \n";
    Status.print_grid status end;
    let bt = !count.bt in
    let propa = !count.propa in
    let sol = !count.sol in
    count := {bt = bt; propa = propa + 1; sol = sol}

(* Appelée à chaque solution *)
let each_sol = fun status all->
    Printf.printf "\nSolution :\n";
    Status.print_grid status;
    let bt = !count.bt in
    let propa = !count.propa in
    let sol = !count.sol in
    count := {bt = bt; propa = propa; sol = sol +1};
    print_count !count all


let rec bt = fun status all print_bt print_propa ->

    let status_saved = save status in (* sauvegarde du statut *)

    if Status.is_queue_empty status then (* Si la file est vide on a une solution *)
    (* Ici on veut toutes les solutions donc on backtrack pour trouver les autres solutions *)
    if all then begin each_sol status all; raise (Back true) end
    (* Sinon on leve une erreur pour arreter le programme à cette solution *)
    else begin each_sol status all; failwith "Solution" end

    else
        let var = Status.select_var status in

        (* On parcourt le domain de la variable sélectionnée *)
        let rec run = fun status domain ->
            match domain with
            (* On a parcouru tout le domaine sans trouver la solution *)
            [] -> each_bt status print_bt; raise (Back true) (* On doit remonter dans l'arbre *)

            (* On teste la propagation sur chaque mot du domaine *)
            | word :: remain_domain ->
                let (propa_result, status_apres_propa) = Propagation.propagation status var word in
                if propa_result then begin
                    each_propa status print_propa;
                    (* Dans tous les cas on evalue 'bt status_apres_propa' ie on plonge dans l'arbre *)
                    (* si true alors c'est qu'apres avoir plongé dans l'arbre on veut backtracker sur ce noeud -> il suffit d'abandonner le mot fixé précedement pour passer au mot suivant *)
                    if bt status_apres_propa all print_bt print_propa then begin Status.delete status_saved var word; run status_saved remain_domain end end (* c'est cette ligne qui gere la pile des appels recursifs *)
                else begin
                    run status_apres_propa remain_domain end
            in
        try
        run status var.domain;
        false (* On arrive jamais ici (run ne peut que lever une exception Back) mais il fautun booleen pour la compilation *)

        (* Si on parcourt tout le domain sans propagation reussie alors il faut backtracker ie bt doit renvoyer true *)
        with Back bl -> bl
