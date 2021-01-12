(*MAIN*)


open Status

let () =

(* test si une chaine de caractere est un fichier .txt *)
let test_txt = fun str ->
    let split = String.split_on_char '.' str in
    (List.length split > 1) && (List.nth split 1 = "txt")
    in

let nom_grille = if (Array.length Sys.argv > 1) && (test_txt Sys.argv.(1)) then Sys.argv.(1) else "exemple.txt" in

let all = Array.mem "all" Sys.argv in (* Est ce que "all" est dans les arguments de la commande *)
let print_bt = Array.mem "print_bt" Sys.argv in
let print_propa = Array.mem "print_propa" Sys.argv in


let dico_name = "dico_fr.txt" in

let status = Reader.read nom_grille dico_name in
try
if Bt.bt status all print_bt print_propa then begin (* bt ne peut que renvoyer true ou lever une exception *)
if all then Printf.printf "\nToutes les solutions ont été trouvées \n"
else Printf.printf "\nIl n'y a pas de solution à cette grille \n"
end

with e -> raise e