(*MAIN*)


open Status

let () = 

let nom_grille = "exemple1.txt" in (* TODO à passer en parametre obligatoire *)
let dico_name = "dico_fr.txt" in (* TODO à passer en parametre optionel avec le francais par defaut *)

let status = Reader.read nom_grille dico_name in

if Bt.bt status then failwith "il n'y a pas de solution à cette grille"



