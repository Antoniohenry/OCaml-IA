type grid = bytes
type direction = Horizontal | Vertical
type domain = string list
type variable = {
  id : int;
  coord : int * int;
  length : int;
  direction : direction;
  mutable domain : domain;
  mutable crossing : int list
}
type status = { grid : grid; mutable vars : variable list; mutable queue : int list }
val update : status -> string -> variable -> unit
val print_grid : status -> unit
val print_var : variable -> unit
val print_vars : variable list -> unit
val get_domain : variable -> domain
val update_queue : status -> unit
val select_var : status -> bool * variable
val get_var : status -> int -> variable
val get_neighbour : status -> int -> int list
val get_queue : status -> int list
val is_queue_empty : status -> bool
val get_element : status -> int
val set_domain : status -> variable -> domain -> unit
val get_crossed : variable -> int list
val set_var : int -> int * int -> int -> direction -> domain -> variable
val copy : status -> status
val copy_queue : int list -> int list
val print_queue : int list -> unit
val reduce_queue : status -> int -> unit
val copy_var : variable -> variable
val delete : status -> variable -> string -> unit
val get_id : variable -> int