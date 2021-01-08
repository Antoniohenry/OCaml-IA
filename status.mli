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

(* Fonctions d'affichage *)
val print_grid : status -> unit
val print_var : variable -> unit
val print_vars : variable list -> unit
val print_queue : int list -> unit

val copy : status -> status

(* Getter et Setter*)
val get_domain : variable -> domain
val get_id : variable -> int
val get_var : status -> int -> variable (* Renvoie UNE variable *)
val get_crossed : variable -> int list (* Renvoie la liste des id des mots croissés *)

val select_var : status -> variable (* Renvoie la premiere variable de queue *)
val set_var : int -> int * int -> int -> direction -> domain -> variable


val update_queue : status -> unit
val update : status -> string -> variable -> unit (* Instanciation d'un mot à une variable *)

val is_queue_empty : status -> bool
val delete : status -> variable -> string -> unit (* Supression d'un mot ans le domain d'un variable  *)
