type direction = Horizontal | Vertical

type variable = {
  id : int;
  coord : int * int; (* ligne * colonne *)
  length : int;
  direction : direction;
  domain : string list; (* Type domain *)
  mutable crossing : int list;
}

(* foction d'affichage des variables *)
val print_vars : variable list -> unit

(* Fonction d'affichage de la grille *)
val print_grid : string -> unit

(* Extrait les variables du ficher .txt correspondant *)
val get_vars_from_txt : string -> variable list
