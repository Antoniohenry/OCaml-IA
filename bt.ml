(* sauvegarde le statut *)
let save = fun status ->
    (*Oo.copy status*)


    let t = Obj.repr status in
    Obj.obj (Obj.dup t)

    (*
	(*let saved_grid = Array.copy status.grid and saved_rvi = Array.copy status.rvi in *)
	let copy = fun t ->
	{t with x = t.x} in
	(*let saved_variables = copy status.variables in *)
	copy status;; *)

let rec bt = fun status ->

    let status_copy = save status in (* sauvegarde du statut *)
    Printf.printf "grille copiée : \n"; Status.print_grid status_copy;

    let (continue, var) = Status.select_var status_copy in

    if not continue then begin Printf.printf "queue vide  \n"; false end
    else
        let rec run_throught = fun domain ->
            if Dico.is_empty domain then begin Printf.printf "domaine vide \n"; false end (* vérification de la taille du domaine *)
            else
                let (word, remain_domain) = Dico.next domain in

                let (propa_result, status_copy) = Propagation.propagation status_copy var word in (* recuperation de la propagation *)
                if propa_result then (bt status_copy) (* rappel du BT avec le noueau statut *)
                    (* TODO plus tard pour tte solution : ici rajouter bt status sur le remain domain pour tester toutes les solutions *)
                else begin
                    var.domain <- remain_domain;
                    Status.set_domain status_copy var remain_domain;
                    Printf.printf "ancienne grille : \n"; Status.print_grid status;
                    bt status end in (* rappel du BT avec l'ancien statut et le nouveau domaine *)
        run_throught (Status.get_domain var)






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


