(* sauvegarde le statut *)
let save = fun status ->
        Status.copy status


let rec bt = fun status ->

    let status_saved = save status in (* sauvegarde du statut *)
    (*Printf.printf "grille sauvegardée: \n"; Status.print_grid status_saved; *)

    let (continue, var) = Status.select_var status in

    if not continue then begin Printf.printf "queue vide  \n"; false end
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


