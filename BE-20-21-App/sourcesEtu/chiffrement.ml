open Arbre 

(* Par rapport au sujet, voici les correspondances de types *)
(* 'd : donnee *)
(* 'sd : symbole donnee *)
(* 'sc : symbole_code *)
(* 'c : code *)
type ('d, 'sd, 'sc, 'c) chiffrement = Chiffrement of ('sd 'sc) arbre_chiffrement * ('d -> 'sd list) * ('sc list -> 'c)




let c_int_1 = Chiffrement (arbre1, List.map (fun c -> int_of_char c - int_of_char 'a'), fun l -> List.fold_left (fun acc c -> acc * 10 + c) 0 l)
let c_int_2 = Chiffrement (arbre2, List.map (fun c -> int_of_char c - int_of_char 'a'), fun l -> List.fold_left (fun acc c -> acc * 10 + c) 0 l)
let c_texte_3 = Chiffrement (arbre3, List.map (fun c -> c), fun l -> List.fold_left (fun acc c -> acc ^ String.make 1 c) "" l)
let c_texte_4 = Chiffrement (arbre4, List.map (fun c -> c), fun l -> List.fold_left (fun acc c -> acc ^ String.make 1 c) "" l)



(* dechiffrer : 'c -> ('d,'sd,'sc,'c) chiffrement -> 'd
Déchiffre un code en utilisant l'abre de chiffrement
Paramètre code : le code à déchiffrer
Paramètre arbre : l'arbre de chiffrement
Retour : la donnée déchiffrée
Exception CodeNonValide : si le code ne peut pas être déchiffré avec l'arbre de chiffrement
*)
let dechiffrer code Chiffrement (arbre, f, g) arbre =
  f (Arbre.dechiffrer code arbre) 
  

(*
let%test _ = dechiffrer 123212 c_int_1 = "bac"
let%test _ = dechiffrer 123 c_int_1 = "ba"
let%test _ = try let _ = dechiffrer 321321 c_int_1 in false with Arbre.CodeNonValide -> true

let%test _ = dechiffrer 123212 c_int_2 = "abcbab"
let%test _ = dechiffrer 123 c_int_2 = "abc"
let%test _ = dechiffrer 321321 c_int_2 = "cbacba"
let%test _ = try let _ = dechiffrer 457 c_int_2 in false with Arbre.CodeNonValide -> true

let%test _ = dechiffrer "au" c_texte_3 = "a"
let%test _ = dechiffrer "aa" c_texte_3 = "b"
let%test _ = dechiffrer "e" c_texte_3 = "c"
let%test _ = dechiffrer "aaaue" c_texte_3 = "bac"

let%test _ = dechiffrer "fea" c_texte_4 = "bac"
let%test _ = dechiffrer "eeeee" c_texte_4 = "aaaaa"
let%test _ = dechiffrer "abc" c_texte_4 = "cde"
let%test _ = dechiffrer "d" c_texte_4 = "f"
*)

(* chiffrer : 'd -> ('d,'sd,'sc,'c) chiffrement -> 'c
chiffre une donnée à l'aide d'un arbre de chiffrement
Paramètre donnee : la donnée à chiffrer
Paramètre arbre : l'arbre de chiffrement
Retour : le code associé à la donnée
Exception DonneeNonValide : si la donnéee ne peut pas être chiffrée avec l'arbre de chiffrement 
*)
let chiffrer donne Chiffrement (arbre, f, g) arbre =
  Arbre.chiffrer (f donnee) arbre

(*
let%test _ = chiffrer "bac" c_int_1 = 123212
let%test _ = chiffrer "ba" c_int_1 = 123

let%test _ = chiffrer "abcbab" c_int_2 = 123212
let%test _ = chiffrer "abc" c_int_2 = 123
let%test _ = chiffrer "cbacba" c_int_2 = 321321
let%test _ = try let _ = chiffrer "dab" c_int_2 in false with Arbre.DonneeNonValide -> true 
let%test _ = try let _ = chiffrer "zut" c_int_2 in false with Arbre.DonneeNonValide -> true

let%test _ = chiffrer "bac" c_texte_3 = "aaaue"

let%test _ = chiffrer "bac" c_texte_4 = "fea"
*)