let rec bt = fun status -> 
        if is_rvi_empty status then true  (* rvi = remain_variables_indexes, liste des index des variables a instancier *)
	else let selected_var_index = choose_var_index (get_rvi status) in (* choix de la variable *)
		let selected_var = get_var status selected_var_index in (*récupération de la variable *)

                        let rec run_throught = fun domain ->
                                if check_domain_empty domain then false (* vérification de la taille du domaine *)
                                else 
                                        let status_copy = save status in
                                        let (word, remain_domain) = Dico.next domain in

                                        let status_copy = save status in (* sauvegarde du statut *)
		                        let propa_result = Propagation.propagation status_copy selected_var_index word in (* recuperation de la propagation *)
                                                if propa_result then (bt status_copy) (* rappel du BT avec le noueau statut *)
                                                (* TODO plus tard pour tte solution : ici rajouter bt status sur le remain domain pour tester toutes les solutions *)
                                                        
                                        else 
                                                status.variables.(selected_var).domain <- remain_domain;
				                bt status in (* rappel du BT avec l'ancien statut et le nouveau domaine *)
                        run_throught (get_domain selected_var)




(* boucle for sur les mots du dico sans else bt  *)
(* ou comme ça *)







(* sauvegarde le statut *)
let save = fun status ->
	(*let saved_grid = Array.copy status.grid and saved_rvi = Array.copy status.rvi in *)
	let copy = fun t ->
	{t with x = t.x} in
	(*let saved_variables = copy status.variables in *)
	copy status;;








(* TODO essayer de rendre plus abstrait le bt donc mettre toutes les fonctions d'accès à des éléments dans un autre fichier comme l'accès à variables ou grille.
   TODO Rendre nos structures mutables pour accèder et modifier en place  
   TODO Potentiellement mettre que grille et variables dans status
   TODO Deplacer des fonctions de check domain empty dans propa
   
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


