(* instancie une variable à un mot dans un status donné
renvoie true/false ie propagation reussie/echouée et le nouveau status correspondant *)
val propagation : Status.status -> Status.variable -> string -> bool * Status.status
