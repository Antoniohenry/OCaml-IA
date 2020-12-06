val init : grid_type -> variable list -> (variable list, remain_var_indexes)
val propagation : global_status -> int -> string -> (bool, global_status)
val grid_update : grid_type -> variable -> string -> grid
val variables_update : grid_type -> variable list -> int -> variable list
val remain_var_update : remain_var_indexes -> variable -> remain_var_indexes

