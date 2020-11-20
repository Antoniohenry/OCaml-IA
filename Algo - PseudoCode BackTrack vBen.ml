(* Chargement des données nécessaires à l'algo *)

(*On a une grille nommé Grille *)
(* On genere une liste de l'ensemble des mots de la grille (structure word) et qu'on note words *)
(* word = structure { id: int, coord_debut: int*int ,  coord_fin: int*int, liste de l'index des word croisés par ce mot: int list }  *)
(* on appelle ensuite une fonction qui cherche les voisins de chaque word dans la grille et rajoute leurs id dans chaque liste correspondante *)

(* on a une fonction create_dico(words) qui genere un dictionnaire de mots compatibles avec le premier word de la liste words donnée (taille et caracteres presents) *)
(* ou une liste vide si words est vide *)


(*1er appel de la fonction *)

algo(Grille,words,create_dico(words))  (*on prend une grille initiale, liste de mots à visiter, dictionnaire associé au premier mot de words si il existe *)


(*Algo*)

(* Si la liste de noeuds/words est vide, il n'y a pas de noeud/word à visiter, alors on a atteint une feuille de l'arbre, la grille est remplie *)
  (* On prévient que c'est bon : True *)

(* Sinon, on prend le premier word à visiter de la liste : on voit donc un nouveau noeud, et on verifie si il y a des branches partant de ce mot *)
(* Les branches sont ici données par les termes du dico compatibles avec ce word/noeud à visiter *)

  (* Si il n'y a pas de termes compatibles, ce noeud n'a pas de branches donc pas de feuilles *)
  (* On prévient que ce n'est pas un bon noeud : False *)

  (* Sinon, il y a au moins un terme compatible/ une branche, on fixe le premier terme compatible de la liste dans la grille c'est à dire qu'on prend cette branche *)
  (* on considère qu'on a visité le noeud précédent et donc on enleve le noeud/word de la liste des noeuds/words à visiter *)

  (* On vérifie alors si en partant dans cette branche, on peut atteindre une feuille : *)
  (* on applique le meme algorithme en donnant en paramètre la grille avec notre branche choisie dedans et la nouvelle liste des noeuds/words à visiter (réorganisée) *)
  (* Et le dictionnaire associé à cette nouvelle liste de noeuds *)
  
  (* si l'algorithme trouve bien que cette branche donne accès à une feuille : grille remplie (True) on renvoie une reponse positive True *)
  (* Sinon, cette branche ne mène à aucune feuille on revient au noeud précédent : réinitialise la grille et la liste des mots *)
    
    (* Si il n'y a pas d'autre feuilles partant de ce noeud (termes compatibles avec ce noeud/word *)
      (* On renvoie que ce noeud ne mène à aucune feuille : False *)

    (* Si il y a encore une autre feuille partant de ce noeud (donc qu'il y avait au moins deux termes compatibles au depart avec le noeud *)
      (* On réapplique l'algorithme avec l'ancienne grille, la liste de mot à visiter (réinitialisés) et le dictionnaire auquel on a enlevé la dernière branche *)
      (* et on renvoie le résultat pour savoir si en revenant au noeud puis en prenant une nouvelle branche, on atteint une feuille *)





(*Pseudo Code *)


algo(Grille, words, dico):        (* Noeud : { liste words de word (mot de la grille) + grille } , mots compatibles du dico : branches partant du noeud *)
                                       
       
        
        
        if words == vide:      (* plus de noeuds à parcourir, on a atteint la feuille de l'arbre *)          
                return True   (* =  on a complété tous les mots de la liste des mots à remplir, on renvoie True ; grille remplie *)
                        
             
        else :                  (* il reste des mots à parcourir *)
                if dico == vide:      (* si le dictionnaire n'a pas de mot compatible, on renvoie False, ce noeud n'a pas de nouvelle branche, on n'atteint aucune feuille *) 
                        return False
                       
                else :        (*dico non vide donc noeud prometteur = nouvelles branches possibles *)
                        (*sauvegarde de grille et mot actuels pour pouvoir retourner a ce noeud si nécessaire *)
                        grille_saved = Grille.copy
                        words_saved = words.copy
                        dico_saved = dico.copy
                        
                        (* prepare la nouvelle branche vers le nouveau noeud / nouvelle itération *)
                        mot_choisi = dico(0)            (*on choisit le premier terme du dico de mots compatibles *)
                        Grille = complete_grid(Grille, words[0], mot_choisi) (*complete la grille avec le nouveau mot *)
                        new_words = enlever_mot(words,word[0])   (*enleve le Word dans la liste de words à traiter*)
                        new_words = reorganisation(new_words, word[0]) (*on reorganise les branches suivantes à traiter / voisins de word mis en premier dans la liste words grace à leur id *)
                                (* on choisit alors la première branche donc le premier word de words (liste des branches réorganisées) *)
                        new_dico = create_dico(new_words[0])               (* on recherche les mots compatibles avec le prochain noeud à traiter *)
                        

                        if algo(Grille, new_words, new_dico)==False :         (* On analyse alors ce nouveau noeud, et si toute nouvelle branche de ce noeud ne va pas jusquaux feuilles: *)

                                if len(dico_saved) >1 :       (* et qu'il reste des mots compatibles dans dico *)
                                        dico_saved = dico_saved[1:]          (* on enleve celui qu'on a testé (la branche et le noeud ou on est) *)
                                        return algo(Grille_saved, words_saved, dico_saved)          (* et on revient au noeud précédent pour prendre une nouvelle branche *)
                                        (*et verifier si le nouveau noeud fonctionne *)  

                                
                                else :                 (* et qu'on a aucun autre mot compatible pour ce noeud, on renvoie False et on revient au noeud précédent *)
                                        Grille = Grille_saved
                                        return False

                        else :
                                return True (* Si cette nouvelle branche atteint une feuille on renvoie True *)
