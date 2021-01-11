(*MAIN*)


open Status

let () = 
let all = ref False in
let nom_grille = if (Array.length Sys.argv)=1 then begin
    if (Sys.argv.(1) = "all") then  all := True ; "exemple.txt" else Sys.argv.(1) end
else all := True ; Sys.argv.(1) in

let dico_name = "dico_fr.txt" in

try
let status = Reader.read nom_grille dico_name in
if Bt.bt status all then failwith "il n'y a pas de solution à cette grille"
with e -> raise e;;



