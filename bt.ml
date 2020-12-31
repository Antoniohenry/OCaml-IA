(* sauvegarde le statut *)
let save = fun status ->
        Status.copy status

exception Back of bool (* Permet de savoir si on remonte dans l'arbre *)


let rec bt = fun status ->

    Printf.printf " \n \nnouveau bt \n";
    Status.print_grid status;
    Status.print_vars status.vars;

    let status_saved = save status in (* sauvegarde du statut *)
    (*Printf.printf "grille sauvegardée: \n"; Status.print_grid status_saved; *)

    if Status.is_queue_empty status then begin (Status.print_grid status); failwith "Solution" end
    else
        let (_, var) = Status.select_var status in (* TODO changer select_var pour renvoyer que var *)
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



 (*
    if not continue then begin Printf.printf "domaine vide  \n"; false end
    else begin
        Status.print_queue status.queue;
        let rec run_throught = fun domain -> (* TODO recursion sert a rien car appel bt recursif *)
            if Dico.is_empty domain then begin Printf.printf "domaine vide \n"; false end (* vérification de la taille du domaine *)
                else begin
                Printf.printf "longueur domaine : %d \n"  (List.length domain);
                let (word, remain_domain) = Dico.next domain in
                Printf.printf "longueur remain domain : %d \n" (List.length remain_domain);
                let (propa_result, status) = Propagation.propagation status  var word in
                 (*Printf.printf "grille copiée après propagation : \n"; Status.print_grid status; *)
                (* recuperation de la propagation *)
                if propa_result then begin
                        Printf.printf "grille après propagation réussie : \n"; Status.print_grid status;
                        (bt status) end (* rappel du BT avec le noueau statut *)
                        (* TODO plus tard pour tte solution : ici rajouter bt status sur le remain domain pour tester toutes les solutions *)

                else begin
                    (*var.domain <- remain_domain;*)
                    Status.set_domain status_saved var remain_domain;
                    Printf.printf "grille après propagation echouée: \n"; Status.print_grid status_saved;
                    bt status_saved
                    end
                end
         in(* rappel du BT avec l'ancien statut et le nouveau domaine *)
        run_throught (Status.get_domain var)
        end


*)



(* TODO essayer de rendre plus abstrait le bt donc mettre toutes les fonctions d'accès à des éléments dans un autre fichier comme l'accès à variables ou grille.
   TODO Rendre nos structures mutables pour accèder et modifier en place  
   TODO Potentiellement mettre que grille et variables dans status
   
*)


(*BT(grille, variables, liste_recherche)

    si la taille(liste_recherche) = 0 -> on a une solution
    sinon : choix de la variable en premier dans la liste 
    liste deja triée en fonction de la priorité

    si taille(domaine_de_la_variable choisie) == 0 -> pas de solution à la grille

    for mot in domaine

        sauvegarder la grille + liste_recherche + variables

        appel de la fonction de propagation(grille, variable, mot, liste_recherche) :
        qui retourne booleen, nouvelle_grille, nouvelles_variables, nouvelle_liste_recherche
            si true -> BT(nouvelle_grille, nouvelles_variables, nouvelle_liste_recherche)
            si false ->
                    on enleve le mot du domaine de la variable choisie -> variables_modifiees
                    + BT(grille_sauvgardée, variables_modifiees, liste_recherche_sauvgardée)
		    *)


