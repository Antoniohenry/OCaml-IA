type domain = string list

(* affiche un domain *)
val print : domain -> unit

(* file_name, max_length -> tableau des domaines non filtres *)
val get_dico : string -> int -> domain array

(* domain, letter, index (commencant à 0) -> domaine filtre *)
val filter : domain -> char -> int -> domain

val is_empty : domain -> bool

val length : domain -> int
