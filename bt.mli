(*pseudo code du BT

variable : { identifiant :int, coordonnee de la premiere lettre : int*int, longueur : int,
 sens : H|V, domaine : type dico du module dico, liste des id des variables croisees }

 grille : char array

 liste_recherche : list of int (identifiant de la variable)
*)
(* global_status = grid, variable list et liste_recherche *)
val save : 'a -> 'b
val bt : Status.status -> bool


 (*BT(grille, variables, liste_recherche)

    si la taille(liste_recherche) = 0 -> on a une solution
    sinon : choix de la variable avec la plus grande priorite

    si taille(domaine_de_la_variable choisie) == 0 -> pas de solution à la grille

    for mot in domaine

        sauvegarder la grille + liste_recherche + variables

        appel de la fonction de propagation(grille, variable, mot, liste_recherche) :
        qui retourne booleen, nouvelle_grille, nouvelles_variables, nouvelle_liste_recherche
            si True -> BT(nouvelle_grille, nouvelles_variables, nouvelle_liste_recherche)
            si False ->
                    on enleve le mot du domaine de la variable choisie -> variables_modifiees
                    + BT(grille_sauvgardée, variables_modifiees, liste_recherche_sauvgardée)
		    *)


