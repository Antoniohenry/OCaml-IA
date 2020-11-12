
(*On a une grille nommé Grille *)
(* On appelle la fonction qui genere une liste de l'ensemble des mots de la grille *)
(* mots = structure { id: int, coord_debut: int ,  coord_fin: int, liste de l'index des mots croisés par ce mot: int list }  *)
(* on appelle ensuite une fonction qui cherche les voisins de chaque mot et rajoute leurs id dans chaque liste correspondante *)

(* on a une fonction create_dico(word) qui genere un dictionnaire de mots compatibles avec le word donné (taille et caracteres presents) *)

(*1er appel de la fonction *)

algo(Grille,words,create_dico(words[0]))  (*on prend une grille initiale, liste de mots, dictionnaire, mot choisi *)


(*PseudoCode *)

(* On choisit un noeud initial avec une grille initiale et liste de mots à visiter *)
(* on applique l'algo : *)
(* Si il n'y a pas de mot à visiter dans la liste c'est a dire pas de nouveaux noeuds, on a atteint une feuille de mon arbre, la grille est remplie *)
(* Sinon, on prend le premier mot à visiter de la liste : on voit donc un nouveau noeud, et on verifie si il y a des branches partant de ce mot *)
(* Les branches sont ici données par les mots du dico compatibles avec ce mot/noeud à visiter *)
(* Si il n'y a pas de mots compatibles, on ne va pas visiter le en renvoyant False et on considère le 
(* On prend le premier terme de la liste des mots compatibles  *)






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
