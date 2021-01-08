(*MAIN*)


open Status

let () = 

let nom_grille = "exemple2pb.txt" in (* TODO à passer en parametre obligatoire *)
let dico_name = "dico_fr.txt" in (* TODO à passer en parametre optionel avec le francais par defaut *)

let status = Reader.read nom_grille dico_name in

Printf.printf "%b \n" (Bt.bt status)



