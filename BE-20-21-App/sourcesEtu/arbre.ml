(* Exception levée quand le code ne peut pas être déchiffré avec l'arbre de chiffrement *)
exception CodeNonValide

(* Exception levée quand la donnée ne peut pas être chiffrée avec l'arbre de chiffrement *)
exception DonneeNonValide

(* Arbre de chiffrement : arbre n-aire avec données de type 'b dans les branches et 'f dans les feuilles *)
type ('b, 'f) arbre_chiffrement = Noeud of ('b * ('b,'f) arbre_chiffrement) list | Lettre of 'f

(* Arbre de chiffrement de la figure 1 du sujet *)


(* Arbre de chiffrement de la figure 1 du sujet *)
let arbre1 =
  Noeud
    [
      (1, Noeud [ (1, Lettre 'e'); (2, Lettre 'b') ]);
      ( 2,
        Noeud
          [ (1, Noeud [ (2, Lettre 'c'); (3, Lettre 'f') ]); (2, Lettre 'd') ]
      );
      (3, Lettre 'a');
    ]

(* Arbre de chiffrement de la figure 2 du sujet *)
let arbre2 = Noeud [ (1, Lettre 'a'); (2, Lettre 'b'); (3, Lettre 'c') ]

(* Arbre de chiffrement de la figure 3 du sujet *)
let arbre3 =
  Noeud
    [
      ('a', Noeud [ ('u', Lettre 'a'); ('a', Lettre 'b') ]);
      ('e', Lettre 'c');
      ('i', Noeud [ ('o', Lettre 'd'); ('u', Lettre 'e'); ('e', Lettre 'f') ]);
    ]

(* Arbre de chiffrement de la figure 4 du sujet *)
let arbre4 =
  Noeud
    [
      ('a', Lettre 'c');
      ('b', Lettre 'd');
      ('c', Lettre 'e');
      ('d', Lettre 'f');
      ('e', Lettre 'a');
      ('f', Lettre 'b');
    ]


(*  getBranche : 'b ->  ('b,'f) arbre_chiffrement -> ('b,'f) arbre_chiffrement option**)
(*  Cherche la branche qui est étiquetée par le premier paramètre et renvoie le sous-arbre associé *)
let getBranche b arbre =
  match arbre with
  | Noeud l -> List.assoc_opt b l
  | Lettre _ -> None



let%test _ =
  getBranche 1 arbre1 = Some (Noeud [ (1, Lettre 'e'); (2, Lettre 'b') ])
let%test _ = getBranche 2 arbre2 = Some (Lettre 'b')
let%test _ = getBranche 4 arbre2 = None
let%test _ =
  getBranche 'i' arbre3
  = Some (Noeud [ ('o', Lettre 'd'); ('u', Lettre 'e'); ('e', Lettre 'f') ])
let%test _ = getBranche 'd' arbre4 = Some (Lettre 'f')
let%test _ = getBranche 'x' arbre4 = None


(*  dechiffrer : 'b list -> ('b,'f) arbre_chiffrement -> 'f list **)
(*  transforme un code (sous forme de liste des symboles de l'alphabet des code) à l'aide d'un arbre **)
(*  renvoie la donnée déchiffrée, sous forme de liste de symboles de l'alphabet des données **)
(*  Exception CodeNonValide : si le code ne peut pas être déchiffré avec l'arbre de chiffrement**)
let rec dechiffrer code arbre_dec =
  match code with
    | [] -> []
    | t::q ->
        match getBranche t arbre_dec with
        | None -> raise CodeNonValide
        | Some sous_arbre -> 
            match sous_arbre with
            | Noeud _ -> dechiffrer q sous_arbre
            | Lettre f -> f::dechiffrer q arbre_dec


let%test _ = dechiffrer [ 1; 2; 3; 2; 1; 2 ] arbre1 = [ 'b'; 'a'; 'c' ]
let%test _ = dechiffrer [ 1; 2; 3 ] arbre1 = [ 'b'; 'a' ]
let%test _ = dechiffrer [ 1; 2; 3; 2; 1; 2 ] arbre2 = [ 'a'; 'b'; 'c'; 'b'; 'a'; 'b' ]
let%test _ = dechiffrer [ 1; 2; 3 ] arbre2 = [ 'a'; 'b'; 'c' ]
let%test _ = dechiffrer [ 3; 2; 1; 3; 2; 1 ] arbre2 = [ 'c'; 'b'; 'a'; 'c'; 'b'; 'a' ]
let%test _ = dechiffrer [ 'a'; 'u' ] arbre3 = [ 'a' ]
let%test _ = dechiffrer [ 'a'; 'a' ] arbre3 = [ 'b' ]
let%test _ = dechiffrer [ 'e' ] arbre3 = [ 'c' ]
let%test _ = dechiffrer [ 'a'; 'a'; 'a'; 'u'; 'e' ] arbre3 = [ 'b'; 'a'; 'c' ]
let%test _ = dechiffrer [ 'f'; 'e'; 'a' ] arbre4 = [ 'b'; 'a'; 'c' ]

let%test _ =
  try
    let _ = dechiffrer [ 3; 2; 1; 3; 2; 1 ] arbre1 in
    false
  with CodeNonValide -> true

let%test _ =
  try
    let _ = dechiffrer [ 4; 5; 7 ] arbre2 in
    false
  with CodeNonValide -> true



(*  arbre_to_list : ('b,'f) arbre_chiffrement -> ('f * 'b list ) list **)
(*  Converti un arbre de chiffrement en une liste associative (symbole de l'alphabet des données, code - liste de symboles de l'alphabet des codes)**)
let rec arbre_to_liste arbre =
  match arbre with
  | Noeud n -> List.flatten (List.map (fun (b, a) -> List.map (fun (f, l) -> (f, b::l)) (arbre_to_liste a)) n)
  | Lettre f -> [(f, [])]

(* [eq_perm l l'] retourne true ssi [l] et [l']
   sont égales à à permutation près (pour (=)).
   [l'] ne doit pas contenir de doublon. *)
let eq_prem l l' =
  List.length l = List.length l' && List.for_all (fun x -> List.mem x l) l'


let%test _ =
  eq_prem (arbre_to_liste arbre1)
    [
      ('e', [ 1; 1 ]);
      ('b', [ 1; 2 ]);
      ('c', [ 2; 1; 2 ]);
      ('f', [ 2; 1; 3 ]);
      ('d', [ 2; 2 ]);
      ('a', [ 3 ]);
    ]

let%test _ =
  eq_prem (arbre_to_liste arbre2) [ ('a', [ 1 ]); ('b', [ 2 ]); ('c', [ 3 ]) ]

let%test _ =
  eq_prem (arbre_to_liste arbre3)
    [
      ('a', [ 'a'; 'u' ]);
      ('b', [ 'a'; 'a' ]);
      ('c', [ 'e' ]);
      ('d', [ 'i'; 'o' ]);
      ('e', [ 'i'; 'u' ]);
      ('f', [ 'i'; 'e' ]);
    ]

let%test _ =
  eq_prem (arbre_to_liste arbre4)
    [
      ('a', [ 'e' ]);
      ('b', [ 'f' ]);
      ('c', [ 'a' ]);
      ('d', [ 'b' ]);
      ('e', [ 'c' ]);
      ('f', [ 'd' ]);
    ]


(*  chiffrer : 'f list -> ('b,'f) arbre_chiffrement -> 'b list **)
(*  transforme une donnée (sous forme de liste de symboles de l'alphabet des données)  **)
(*  renvoie le code associé à la donnée, pour l'abre, sous forme de liste de symboles de l'alphabet des codes **)
(*  Exception DonneeNonValide : si la donnéee ne peut pas être chiffrée avec l'arbre de chiffrement **)
let rec chiffrer  liste arbre_chiff =
  match liste with
  | [] -> []
  | t::q ->
      let rec aux l a =
        match l with
        | [] -> raise DonneeNonValide
        | (f, b)::q ->
            if f = t then b
            else aux q a
      in
      aux (arbre_to_liste arbre_chiff) arbre_chiff @ chiffrer q arbre_chiff

let%test _ = chiffrer [ 'b'; 'a'; 'c' ] arbre1 = [ 1; 2; 3; 2; 1; 2 ]
let%test _ = chiffrer [ 'b'; 'a' ] arbre1 = [ 1; 2; 3 ]
let%test _ = chiffrer [ 'a'; 'b'; 'c'; 'b'; 'a'; 'b' ] arbre2 = [ 1; 2; 3; 2; 1; 2 ]
let%test _ = chiffrer [ 'a'; 'b'; 'c' ] arbre2 = [ 1; 2; 3 ]
let%test _ = chiffrer [ 'c'; 'b'; 'a'; 'c'; 'b'; 'a' ] arbre2 = [ 3; 2; 1; 3; 2; 1 ]
let%test _ = chiffrer [ 'b'; 'a'; 'c' ] arbre3 = [ 'a'; 'a'; 'a'; 'u'; 'e' ]
let%test _ = chiffrer [ 'b'; 'a'; 'c' ] arbre4 = [ 'f'; 'e'; 'a' ]

let%test _ =
  try
    let _ = chiffrer [ 'd'; 'a'; 'b' ] arbre2 in
    false
  with DonneeNonValide -> true

let%test _ =
  try
    let _ = chiffrer [ 'z'; 'u'; 't' ] arbre2 in
    false
  with DonneeNonValide -> true 


(*****************************************)
(*  BONUS                               **)
(*****************************************)

  
(*  fold : TO DO *)
let fold  = fun _ -> failwith "TODO"

(*  nbSymboles : ('b,'f) arbre_chiffrement : ('b,'f) arbre_chiffrement -> int
Fonction qui calcule le nombre de symboles de l'alphabet des données présentent dans un arbre de chiffrement
Paramètre : l'arbre de chiffrement
Retour : le nombre de synboles de l'alphabet des données
**)
let nbSymboles  = fun _ -> failwith "TODO"

(*
let%test _ = nbSymboles arbre1 = 6
let%test _ = nbSymboles arbre2 = 3
let%test _ = nbSymboles arbre3 = 6
let%test _ = nbSymboles arbre4 = 6
*)

(*  symboles :  ('b,'f) arbre_chiffrement -> 'f list
Fonction qui renvoie la liste des symboles de l'alphabet des données présents dans un arbre de chiffrement
Paramètre : l'arbre de cryage
Retour : la liste des symboles
**)
let symboles  = fun _ -> failwith "TODO"

(*
let%test _ = eq_prem (symboles arbre1) ['a'; 'd'; 'f'; 'c'; 'b'; 'e']
let%test _ = eq_prem (symboles arbre2) ['c'; 'b'; 'a']
let%test _ = eq_prem (symboles arbre3) ['f'; 'e'; 'd'; 'c'; 'b'; 'a']
let%test _ = eq_prem (symboles arbre4) ['b'; 'a'; 'f'; 'e'; 'd'; 'c']
*)

(*  arbre_to_liste_2 :  ('b,'f) arbre_chiffrement -> ('f * 'b list ) list **)
(*  Converti un arbre de chiffrement en une liste associative (symbole de l'alphabet des données, code - liste de symboles de l'alphabet des codes)**)
let arbre_to_liste_2  = fun _ -> failwith "TODO"

(*
let%test _ =
  eq_prem (arbre_to_liste_2 arbre1)
    [
      ('e', [ 1; 1 ]);
      ('b', [ 1; 2 ]);
      ('c', [ 2; 1; 2 ]);
      ('f', [ 2; 1; 3 ]);
      ('d', [ 2; 2 ]);
      ('a', [ 3 ]);
    ]

let%test _ =
  eq_prem (arbre_to_liste_2 arbre2) [ ('a', [ 1 ]); ('b', [ 2 ]); ('c', [ 3 ]) ]

let%test _ =
  eq_prem (arbre_to_liste_2 arbre3)
    [
      ('a', [ 'a'; 'u' ]);
      ('b', [ 'a'; 'a' ]);
      ('c', [ 'e' ]);
      ('d', [ 'i'; 'o' ]);
      ('e', [ 'i'; 'u' ]);
      ('f', [ 'i'; 'e' ]);
    ]

let%test _ =
  eq_prem (arbre_to_liste_2 arbre4)
    [
      ('a', [ 'e' ]);
      ('b', [ 'f' ]);
      ('c', [ 'a' ]);
      ('d', [ 'b' ]);
      ('e', [ 'c' ]);
      ('f', [ 'd' ]);
    ]
*)