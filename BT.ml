let rec bt = fun status -> 
        if is_rvi_empty status then true  (* rvi = remain_variables_indexes, liste des index des variables a instancier *)
	else let selected_var_index = choose_var_index (get_rvi status) in (* choix de la variable *)
		let selected_var = get_var status selected_var_index in (*récupération de la variable *)
		if check_domain_empty selected_var then false (* vérification de la taille du domaine *)
else let word_list = get_domain selected_var in 
                for word in word_list do (*TODO iter pour une liste *)
			let status_copy = save status in (* sauvegarde du statut *)
			let propa_result = Propagation.propagation status_copy selected_var_index word in (* recuperation de la propagation *)
			if propa_result then bt status_copy (* rappel du BT avec le noueau statut *)
			else 
				domain_update selected_var; (* suppression le premier mot dans le domaine *)
				bt status; (* rappel du BT avec l'ancien statut et le nouveau domaine *)
		done








(*


(* update du domaine en enlevant le mot qui marche pas *)
let domain_update = fun status word -> (* a refaire dans propagation *)
	let new_domain = List.tl variable.domain in (*TODO a changer si le domaine est un array*)
	let variable.domain = new_domain in 
variable
*)


(* sauvegarde le statut *)
let save = fun status ->
	(*let saved_grid = Array.copy status.grid and saved_rvi = Array.copy status.rvi in *)
	let copy = fun t ->
	{t with x = t.x} in
	(*let saved_variables = copy status.variables in *)
	copy status;;

let choose_var_index = fun rvi -> (*on prend la premiere variable de la liste des variables restantes *)
	get_index_rvi rvi

let check_domain_empty = fun variable -> (* on vérifie que le domaine n'est pas vide *)
	match variable.domain with
	Empty -> true (* TODO a modifier suivant le type domaine *)
	|_ -> false



(* TODO essayer de rendre plus abstrait le bt donc mettre toutes les fonctions d'accès à des éléments dans un autre fichier comme l'accès à variables ou grille.
   TODO Rendre nos structures mutables pour accèder et modifier en place  
   TODO Potentiellement mettre que grille et variables dans status
   TODO Deplacer des fonctions de check domain empty dans propa
   
*)


(* TODO Pour modeler on peut faire un tableau de couple de mot croisant avec char croisant ? *)
















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

