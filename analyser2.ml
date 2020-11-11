
(*On a une grille nommé Grille *)
(* On appelle la fonction qui genere une liste de l'ensemble des mots de la grille *)
(* mots = structure { id: int, coord_debut: int ,  coord_fin: int, liste de l'index des mots croisés par ce mot: int list }  *)
(* on appelle ensuite une fonction qui cherche les voisins de chaque mot et rajoute leurs id dans chaque liste correspondante *)

(* on a une fonction create_dico(word) qui genere un dictionnaire de mots compatibles avec le word donné (taille et caracteres presents) *)

(*1er appel de la fonction *)

algo(Grille,words,create_dico(words[0]),words[0])  (*on prend une grille initiale, liste de mots, dictionnaire, mot choisi *)


(*PseudoCode *)

algo(Grille, words, dico, word):

        if dico = vide :
                return False           (* si le dictionnaire n'a pas de mot compatible, on renvoie False, cette branche n'atteint aucune feuille = grille remplie *) 

        else : 
                if words = vide:      (* plus de noeud à parcourir, on a atteint la feuille de l'arbre *)          
                        return True   (* =  on a complété tous les mots de la liste des mots à remplir, on renvoie True *)
                       
                else :        (*dico non vide donc nouveau noeud prometteur = nouvelles branches possibles *)
                        (*sauvegarde de grille et mot actuels pour pouvoir retourner a ce noeud si nécessaire *)
                        grille_saved = Grille.copy
                        words_saved = words.copy

                        (* prepare la nouvelle branche vers le nouveau noeud / nouvelle itération *)
                        mot_choisi = dico(0)            (*on choisit le premier terme du dico de mots compatibles *)
                        Grille = complete_grid(Grille, word, mot_choisi) (*complete la grille avec le nouveau mot *)
                        new_words = enlever_mot(words,word)   (*enleve le Word dans la liste de words à traiter*)
                        new_words = reorganisation(new_words, word) (*on reorganise les branches suivantes à traiter / voisins de word mis en premier dans la liste words grace à leur id *)
                                (* on choisit alors la première branche donc le premier word de words (liste des branches réorganisées) *)
                        new_dico = dico(words[0])               (* on recherche les mots compatibles avec le prochain noeud à traiter *)
                        

                        if algo(Grille, new_words, new_dico, words[0])==False :         (* On analyse alors ce nouveau noeud, et si toute nouvelle branche de ce noeud ne va pas jusquaux feuilles: *)

                                if len(dico) >1 :       (* et qu'il reste des mots compatibles dans dico *)
                                        dico =dico[1:]          (* on enleve celui qu'on a testé (la branche et le noeud ou on est) *)
                                        return algo(Grille_saved, words_saved,dico,words_saved[0])          (* et on revient au noeud précédent pour prendre une nouvelle branche *)
                                        (*et verifier si le nouveau noeud fonctionne *)  

                                
                                else :                 (* et qu'on a aucun autre mot compatible pour ce noeud, on renvoie False et on revient au noeud précédent *)
                                        Grille = Grille_saved
                                        return False

                        else :
                                return True (* Si cette nouvelle branche atteint une feuille on renvoie True *)
