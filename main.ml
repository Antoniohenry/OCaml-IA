(*MAIN*)

(* necessite les modules dictionary, generator, modeler, analyser (,display) *)

open Status

let () = 

(*Creation du dictionnaire de mots tries avec la fonction "generate_dictionary" du module dictionary qui prend en parametre le nom du fichier dico *)
(*let dictionary = Dictionary.generate_dictionary "dico.txt"*)


(*Generation d'une grille de mots croises et la sauvegarde avec la fonction "generate_crossword" du module generator qui prend en parametre le nom fichier.txt *)

(*let nom_grille = "grille.txt"
Generator.generate_crossword nom_grille  (*utile pour plus tard lors de series de tests / pas utile au debut du dev -> grille rentree manuellement pour tester*) 
*)

(*Chargement de la grille sauvegardé avec la fct "model_crossword" du module modeler avec param  "fichier.txt" pour la modeliser en tableau pouvant être analysé *)  
let nom_grille = "exemple.txt" in

let (_, _, grid) = Reader.get_grid nom_grille in

let vars = Reader.get_vars_from_txt nom_grille in
let state = {grid = grid; vars = vars; queue = []} in
Status.update_queue state;
Printf.printf "%b \n" (Bt.bt state)



(*Analyse de la grille modelisee precedemment avec la fct "analyse_crossword" du module analyser avec param le tableau grille *)
(*qui renvoie la meme grille si elle n'est pas valide ou la grille remplie sinon *)


(*let crossword = Analyser.analyse_crossword grid *)


(*Affichage du succès de la grille remplie si elle est valide ou de l'originale sinon *)



