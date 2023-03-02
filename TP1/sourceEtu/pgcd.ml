(*  Exercice à rendre **)
(*  pgcd : int -> int -> int *)
(* renvoie le pgcd de 2 entiers *)
(* a : premier terme*)
(* b : deuxième terme*)
(* renvoie le pgcd entre a et b *)
(* précondition : a && b /= 0 *)
(* postcondition : pgcd > 0 *)
let pgcd a b = 
  (* pgcd_positif : int -> int -> int *)
  (* renvoie le pgcd de 2 entiers positifs *)
  (* a : premier terme *)
  (* b : deuxième terme *)
  (* renvoie le pgcd entre a et b*)
  (* précondition : (a && b/= 0) && a et b ∈ N* *)
  (* Fonction récursive auxiliaire au pgcd *)
  let rec pgcd_positif_aux a b = 
    if (a = 0 || b = 0) then failwith "Les termes doivent être non nuls"
    else
      if a=b
      then a
      else if a>b then
        pgcd_positif_aux (a-b) b 
      else pgcd_positif_aux a (b-a)
    in 
    pgcd_positif_aux (abs a) (abs b)



let%test _ = pgcd 1 1 > 0
let%test _ = pgcd 2 4 = 2
let%test _ = pgcd 2 5 = 1
let%test _ = pgcd (-2) (-6) = 2


