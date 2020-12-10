(* TODO type domain  *)

(* file_name, length -> domain (non filtre) *)
val get_domain : string -> int -> string list
(* Domain_init *)

(* affiche un domain *)
val print : string list -> unit

(* file_name, max_length -> tableau des domaines non flitres *)
val get_dico : string -> int -> string list array

(* domain, letter, index (commencant à 0) -> domaine filtre *)
val filter : string list -> char -> int -> string list

(* TODO fonction verifiant si le domain vide *)
(* val is_empty : domain -> bool *)

(* TODO fonction parcourant le domain *)