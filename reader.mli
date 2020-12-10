(* Extrait les variables du ficher .txt correspondant *)
type variable = Status.variable

val get_grid : string -> int * int * bytes

val get_vars_from_txt : string -> variable list
