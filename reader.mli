(* Extrait le status du ficher .txt correspondant *)

type status = Status.status

(* Nom du fichier, nom du dico, renvoie status *)
val read : string -> string -> status
