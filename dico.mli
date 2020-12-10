type domain = string list


(* file_name, length -> domain (non filtre) *)
val get_domain : string -> int -> string list
(* Domain_init *)

(* affiche un domain *)
val print : string list -> unit

(* file_name, max_length -> tableau des domaines non flitres *)
val get_dico : string -> int -> string list array

(* domain, letter, index (commencant à 0) -> domaine filtre *)
val filter : string list -> char -> int -> string list

val next : 'a list -> 'a * 'a list

val is_empty : 'a list -> bool

val length : domain -> int