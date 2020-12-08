val propagation : global_status -> int -> string -> (bool, global_status)

val grid_update : grid_type -> variable -> string -> grid
val variables_update : grid_type -> variable list -> int -> variable list
val remain_var_update : remain_var_indexes -> variable -> remain_var_indexes

val is_rvi_empty : status -> bool
val get_index_rvi : int list -> int
val get_rvi : status -> int list
val get_var : status -> int -> variable
val get_domain : variable -> domain
val domain_reduce : domain -> unit
val update_domain : grid_type -> int -> int ->bool
val is_domain_empty : variable -> bool



