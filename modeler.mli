type direction_type = H | V (* H pour horizontal et V pour vertical *)
(* ncl = Neighbours Crossing List *)
type neigh_priority = int*int (* identifiant et priorité *) 
type variable = {id : int; start_position : int*int; length : int; direction : direction_type; domain : domain_type; ncl : neigh_priority list } 
type grid_type = char array (* type grille *)
type global_status = { grid : grid_type; variables : variable list; rvi : int list } (* type status contenant la grille, les variables, et les variables restantes a instancier *)
type remain_var_indexes = int list (* identifiants des variables qu'il reste a instancier, la liste est automatiquement triée par priorité *)

val model_crossword : string -> (int, string)
val get_table : (int,string) -> grid_type 
