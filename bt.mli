(* Fonction de deep copy *)
val save : Status.status -> Status.status

(* Parametre : status, afficher toutes les solutions, afficher à chaque backtrack, afficher à chaque propagation, renvoie un booleen déclancheant le bt *)
val bt : Status.status -> bool -> bool -> bool -> bool

