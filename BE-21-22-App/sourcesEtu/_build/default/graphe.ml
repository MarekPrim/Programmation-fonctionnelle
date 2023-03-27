(***************************)
(* Première implémentation *)
(***************************)

(* Graphe orienté representé par une liste de sommet et une liste d'arêtes *)
type 'a graphe_ev = EV of 'a list * ('a * 'a) list

let g0_ev = EV ([], [])

let g1_ev =
  EV
    ( [ 0; 1; 2; 3; 4; 5 ],
      [ (0, 1); (0, 2); (1, 3); (1, 4); (1, 5); (2, 0); (2, 3); (3, 4); (4, 0) ]
    )

let g2_ev =
  EV
    ( [ 0; 1; 2; 3; 4; 5 ],
      [ (0, 2); (0, 4); (1, 5); (2, 0); (2, 3); (3, 4); (5, 1) ] )

let g3_ev =
  EV
    ( [ 'a'; 'b'; 'c'; 'd'; 'e' ],
      [
        ('a', 'b');
        ('a', 'c');
        ('a', 'e');
        ('b', 'e');
        ('c', 'e');
        ('d', 'b');
        ('d', 'c');
        ('d', 'e');
      ] )

let g4_ev = EV ([ 0; 1 ], [ (0, 1) ])

let mechant_edges = EV (["e"], [])

let mechant_aretes = EV ([], [(0,2)])

(*** Exercice 1 ***)
(* graphe_ev -> boolean*)
let bienforme_ev (EV (edges,aretes)) =
  List.for_all (fun (src,tgt) -> List.for_all (fun arete -> List.mem arete edges) [src;tgt]) aretes

let%test _ = bienforme_ev g3_ev = true
let%test _ = bienforme_ev g2_ev = true
let%test _ = bienforme_ev g1_ev = true
let%test _ = bienforme_ev g4_ev = true
let%test _ = bienforme_ev (EV ([], []))
let%test _ = bienforme_ev (EV ([1; 4], []))
let%test _ = bienforme_ev (EV ([], [(7, 8)])) = false
let%test _ = bienforme_ev (EV ([4; 8; 7], [(8, 7); (4, 9)])) = false
let%test _ = bienforme_ev (EV ([4; 8; 7], [(8, 7); (9, 4)])) = false

(* Tests : TO DO *)

(*** Exercice 2 ***)

exception PasUnSommet

(* voisins_ev : 'a -> 'a graphe_ev -> 'a list *)
(* Renvoie la liste des sommets voisins d'un sommet dans une graphe *)
(* Paramètre a : le sommet dont on cherche les voisins *)
(* Paramètre g : le graphe dans lequel on cherche les voisins *)
(* Précondition : le graphe est bien formé *)
(* Résultat : la liste des voisins *)
(* Exception PasUnSommet si le sommet n'est pas un sommet du graphe *)
let voisins_ev edge (EV (edges,aretes)) =
  if List.mem edge edges then
    List.fold_left (fun acc a ->
      match a with (a1,a2)
      when a1 = edge -> a2::acc
      | _ -> acc) [] aretes
  else
    raise PasUnSommet

(* Pour les tests : égalité de deux listes modulo permutation *)
let eq_perm l l' =
  List.length l = List.length l' && List.for_all (fun x -> List.mem x l) l'

let%test _ =
  try
    let _ = voisins_ev 6 g0_ev in
    false
  with PasUnSommet -> true

let%test _ = eq_perm (voisins_ev 0 g1_ev) [ 1; 2 ]
let%test _ = eq_perm (voisins_ev 1 g1_ev) [ 3; 4; 5 ]
let%test _ = eq_perm (voisins_ev 2 g1_ev) [ 0; 3 ]
let%test _ = eq_perm (voisins_ev 3 g1_ev) [ 4 ]
let%test _ = eq_perm (voisins_ev 4 g1_ev) [ 0 ]
let%test _ = eq_perm (voisins_ev 5 g1_ev) []

let%test _ =
  try
    let _ = voisins_ev 6 g1_ev in
    false
  with PasUnSommet -> true

let%test _ = eq_perm (voisins_ev 0 g2_ev) [ 2; 4 ]
let%test _ = eq_perm (voisins_ev 1 g2_ev) [ 5 ]
let%test _ = eq_perm (voisins_ev 2 g2_ev) [ 0; 3 ]
let%test _ = eq_perm (voisins_ev 3 g2_ev) [ 4 ]
let%test _ = eq_perm (voisins_ev 4 g2_ev) []
let%test _ = eq_perm (voisins_ev 5 g2_ev) [ 1 ]

let%test _ =
  try
    let _ = voisins_ev 10 g2_ev in
    false
  with PasUnSommet -> true

let%test _ = eq_perm (voisins_ev 'a' g3_ev) [ 'b'; 'c'; 'e' ]
let%test _ = eq_perm (voisins_ev 'b' g3_ev) [ 'e' ]
let%test _ = eq_perm (voisins_ev 'c' g3_ev) [ 'e' ]
let%test _ = eq_perm (voisins_ev 'd' g3_ev) [ 'b'; 'c'; 'e' ]
let%test _ = eq_perm (voisins_ev 'e' g3_ev) []

let%test _ =
  try
    let _ = voisins_ev 'x' g3_ev in
    false
  with PasUnSommet -> true

let%test _ = eq_perm (voisins_ev 0 g4_ev) [ 1 ]
let%test _ = eq_perm (voisins_ev 1 g4_ev) []

let%test _ =
  try
    let _ = voisins_ev 4 g4_ev in
    false
  with PasUnSommet -> true

(*** Exercice 3 ***)

(* accessible_depuis_ev : 'a -> 'a graphe_ev -> 'a list *)
(* Renvoie la liste des sommets accessible depuis un sommet dans une graphe *)
(* Paramètre a : le sommet depuis lequel on cherche les sommets accessibles *)
(* Paramètre g : le graphe dans lequel on cherche les voisins *)
(* Précondition : le graphe est bien formé *)
(* Résultat : la liste des sommets accessibles *)
(* Exception PasUnSommet si le sommet n'est pas un sommet du graphe *)

let uniq_cons x xs = if List.mem x xs then xs else x :: xs

let remove_from_right xs = List.fold_right uniq_cons xs []

let accessible_depuis_ev edge (EV(edges,aretes)) =
  if not (List.mem edge edges) then
    raise PasUnSommet
  else
    let rec aux_accesible_depuis_ev parcouru restant =
      match restant with
      | [] -> parcouru
      | t::q -> voisins_ev t (EV(edges,aretes)) @ parcouru @ (aux_accesible_depuis_ev (t::parcouru) q)
    in remove_from_right (aux_accesible_depuis_ev [edge] (voisins_ev edge (EV(edges,aretes))))

(* let%test _ =
  try
    let _ = accessible_depuis_ev 6 g0_ev in
    false
  with PasUnSommet -> true

let%test _ = eq_perm (accessible_depuis_ev 0 g1_ev) [ 0; 1; 2; 3; 4; 5 ]
let%test _ = eq_perm (accessible_depuis_ev 1 g1_ev) [ 0; 1; 2; 3; 4; 5 ]
let%test _ = eq_perm (accessible_depuis_ev 2 g1_ev) [ 0; 1; 2; 3; 4; 5 ]
let%test _ = eq_perm (accessible_depuis_ev 3 g1_ev) [ 0; 1; 2; 3; 4; 5 ]
let%test _ = eq_perm (accessible_depuis_ev 4 g1_ev) [ 0; 1; 2; 3; 4; 5 ]
let%test _ = eq_perm (accessible_depuis_ev 5 g1_ev) [ 5 ]

let%test _ =
  try
    let _ = accessible_depuis_ev 6 g1_ev in
    false
  with PasUnSommet -> true

let%test _ = eq_perm (accessible_depuis_ev 0 g2_ev) [ 0; 2; 3; 4 ]
let%test _ = eq_perm (accessible_depuis_ev 1 g2_ev) [ 1; 5 ]
let%test _ = eq_perm (accessible_depuis_ev 2 g2_ev) [ 0; 2; 3; 4 ]
let%test _ = eq_perm (accessible_depuis_ev 3 g2_ev) [ 3; 4 ]
let%test _ = eq_perm (accessible_depuis_ev 4 g2_ev) [ 4 ]
let%test _ = eq_perm (accessible_depuis_ev 5 g2_ev) [ 1; 5 ]

let%test _ =
  try
    let _ = accessible_depuis_ev 10 g2_ev in
    false
  with PasUnSommet -> true

let%test _ = eq_perm (accessible_depuis_ev 'a' g3_ev) [ 'a'; 'b'; 'c'; 'e' ]
let%test _ = eq_perm (accessible_depuis_ev 'b' g3_ev) [ 'b'; 'e' ]
let%test _ = eq_perm (accessible_depuis_ev 'c' g3_ev) [ 'c'; 'e' ]
let%test _ = eq_perm (accessible_depuis_ev 'd' g3_ev) [ 'b'; 'c'; 'd'; 'e' ]
let%test _ = eq_perm (accessible_depuis_ev 'e' g3_ev) [ 'e' ]

let%test _ =
  try
    let _ = accessible_depuis_ev 'x' g3_ev in
    false
  with PasUnSommet -> true

let%test _ = eq_perm (accessible_depuis_ev 0 g4_ev) [ 0; 1 ]
let%test _ = eq_perm (accessible_depuis_ev 1 g4_ev) [ 1 ]

let%test _ =
  try
    let _ = accessible_depuis_ev 4 g4_ev in
    false
  with PasUnSommet -> true *)

(***************************)
(*  Seconde implémentation *)
(***************************)

(* Graphe orienté représenté par une liste de couple (sommet, voisins du sommet) *)
type 'a graphe_s = Sommets of ('a * 'a list) list

let g0_s = Sommets []

let g1_s =
  Sommets
    [
      (0, [ 1; 2 ]);
      (1, [ 3; 4; 5 ]);
      (2, [ 0; 3 ]);
      (3, [ 4 ]);
      (4, [ 0 ]);
      (5, []);
    ]

let g2_s =
  Sommets
    [
      (0, [ 2; 4 ]); (1, [ 5 ]); (2, [ 0; 3 ]); (3, [ 4 ]); (4, []); (5, [ 1 ]);
    ]

let g3_s =
  Sommets
    [
      ('a', [ 'b'; 'c'; 'e' ]);
      ('b', [ 'e' ]);
      ('c', [ 'e' ]);
      ('d', [ 'b'; 'c'; 'e' ]);
      ('e', []);
    ]

let g4_s = Sommets [ (0, [ 1 ]); (1, []) ]

(*** Exercice 4 ***)
(* Paramètre Sommets : Arbre dont il faut vérifier la validité *)
(* 'a graphe_s -> boolean*)
(* TO DO *)
let bienforme_s  (Sommets s) =
  (fun (edges,aretes) -> List.for_all (fun t -> List.mem t edges) (remove_from_right (List.concat aretes))) (List.split s)

(*Tests TO DO*)

let%test _ = bienforme_s g1_s = true
let%test _ = bienforme_s g0_s = true
let%test _ = bienforme_s g2_s = true
let%test _ = bienforme_s g3_s = true
let%test _ = bienforme_s g4_s = true
let%test _ = bienforme_s (Sommets [(0,[1])]) = false

(*  Exercice 5 * )

(* voisins_s : 'a -> 'a graphe_s -> 'a list *)
(* Renvoie la liste des sommets voisins d'un sommet dans une graphe *)
(* Paramètre a : le sommet dont on cherche les voisins *)
(* Paramètre g : le graphe dans lequel on cherche les voisins *)
(* Précondition : le graphe est bien formé *)
(* Résultat : la liste des voisins *)
(* Exception PasUnSommet si le sommet n'est pas un sommet du graphe *)*)
let voisins_s sommet (Sommets(s)) =
  let resultat = List.find_map (fun (edge,voisins) -> if edge=sommet then Some(voisins) else None) s
in match resultat with
 | Some voisins -> voisins
 | None -> raise PasUnSommet

let%test _ =
  try
    let _ = voisins_s 6 g0_s in
    false
  with PasUnSommet -> true

let%test _ = eq_perm (voisins_s 0 g1_s) [ 1; 2 ]
let%test _ = eq_perm (voisins_s 1 g1_s) [ 3; 4; 5 ]
let%test _ = eq_perm (voisins_s 2 g1_s) [ 0; 3 ]
let%test _ = eq_perm (voisins_s 3 g1_s) [ 4 ]
let%test _ = eq_perm (voisins_s 4 g1_s) [ 0 ]
let%test _ = eq_perm (voisins_s 5 g1_s) []

let%test _ =
  try
    let _ = voisins_s 6 g1_s in
    false
  with PasUnSommet -> true

let%test _ = eq_perm (voisins_s 0 g2_s) [ 2; 4 ]
let%test _ = eq_perm (voisins_s 1 g2_s) [ 5 ]
let%test _ = eq_perm (voisins_s 2 g2_s) [ 0; 3 ]
let%test _ = eq_perm (voisins_s 3 g2_s) [ 4 ]
let%test _ = eq_perm (voisins_s 4 g2_s) []
let%test _ = eq_perm (voisins_s 5 g2_s) [ 1 ]

let%test _ =
  try
    let _ = voisins_s 10 g2_s in
    false
  with PasUnSommet -> true

let%test _ = eq_perm (voisins_s 'a' g3_s) [ 'b'; 'c'; 'e' ]
let%test _ = eq_perm (voisins_s 'b' g3_s) [ 'e' ]
let%test _ = eq_perm (voisins_s 'c' g3_s) [ 'e' ]
let%test _ = eq_perm (voisins_s 'd' g3_s) [ 'b'; 'c'; 'e' ]
let%test _ = eq_perm (voisins_s 'e' g3_s) []

let%test _ =
  try
    let _ = voisins_s 'x' g3_s in
    false
  with PasUnSommet -> true

let%test _ = eq_perm (voisins_s 0 g4_s) [ 1 ]
let%test _ = eq_perm (voisins_s 1 g4_s) []

let%test _ =
  try
    let _ = voisins_s 4 g4_s in
    false
  with PasUnSommet -> true

(*** Exercice 6 ***)

(* accessible_depuis_s : 'a -> 'a graphe_s -> 'a list *)
(* Renvoie la liste des sommets accessible depuis un sommet dans une graphe *)
(* Paramètre a : le sommet depuis lequel on cherche les sommets accessibles *)
(* Paramètre g : le graphe dans lequel on cherche les voisins *)
(* Précondition : le graphe est bien formé *)
(* Résultat : la liste des sommets accessibles *)
(* Exception PasUnSommet si le sommet n'est pas un sommet du graphe *)
let rec and_fold f list = match list with
| [] -> true
| hd::tl -> 
  if f hd
  then and_fold f tl
  else false
  let accessible_depuis_s start (Sommets(sommets))= 
  let rec aux acc fut = 
    match fut with 
    | [] -> acc
    | hd::tl -> 
      let nexts = List.filter (fun x -> not ((List.exists (fun y -> y=x) acc) || (List.exists (fun y -> y=x) fut) )) (voisins_s hd (Sommets(sommets))) in
      aux (hd::acc) (nexts@tl)
  in
  if and_fold (fun (n,_) -> n <> start) sommets 
    then raise PasUnSommet
    else aux [] [start]

let%test _ =
  try
    let _ = accessible_depuis_s 6 g0_s in
    false
  with PasUnSommet -> true

let%test _ = eq_perm (accessible_depuis_s 0 g1_s) [ 0; 1; 2; 3; 4; 5 ]
let%test _ = eq_perm (accessible_depuis_s 1 g1_s) [ 0; 1; 2; 3; 4; 5 ]
let%test _ = eq_perm (accessible_depuis_s 2 g1_s) [ 0; 1; 2; 3; 4; 5 ]
let%test _ = eq_perm (accessible_depuis_s 3 g1_s) [ 0; 1; 2; 3; 4; 5 ]
let%test _ = eq_perm (accessible_depuis_s 4 g1_s) [ 0; 1; 2; 3; 4; 5 ]
let%test _ = eq_perm (accessible_depuis_s 5 g1_s) [ 5 ]

let%test _ =
  try
    let _ = accessible_depuis_s 6 g1_s in
    false
  with PasUnSommet -> true

let%test _ = eq_perm (accessible_depuis_s 0 g2_s) [ 0; 2; 3; 4 ]
let%test _ = eq_perm (accessible_depuis_s 1 g2_s) [ 1; 5 ]
let%test _ = eq_perm (accessible_depuis_s 2 g2_s) [ 0; 2; 3; 4 ]
let%test _ = eq_perm (accessible_depuis_s 3 g2_s) [ 3; 4 ]
let%test _ = eq_perm (accessible_depuis_s 4 g2_s) [ 4 ]
let%test _ = eq_perm (accessible_depuis_s 5 g2_s) [ 1; 5 ]

let%test _ =
  try
    let _ = accessible_depuis_s 10 g2_s in
    false
  with PasUnSommet -> true

let%test _ = eq_perm (accessible_depuis_s 'a' g3_s) [ 'a'; 'b'; 'c'; 'e' ]
let%test _ = eq_perm (accessible_depuis_s 'b' g3_s) [ 'b'; 'e' ]
let%test _ = eq_perm (accessible_depuis_s 'c' g3_s) [ 'c'; 'e' ]
let%test _ = eq_perm (accessible_depuis_s 'd' g3_s) [ 'b'; 'c'; 'd'; 'e' ]
let%test _ = eq_perm (accessible_depuis_s 'e' g3_s) [ 'e' ]

let%test _ =
  try
    let _ = accessible_depuis_s 'x' g3_s in
    false
  with PasUnSommet -> true

let%test _ = eq_perm (accessible_depuis_s 0 g4_s) [ 0; 1 ]
let%test _ = eq_perm (accessible_depuis_s 1 g4_s) [ 1 ]

let%test _ =
  try
    let _ = accessible_depuis_s 4 g4_s in
    false
  with PasUnSommet -> true

(***************************)
(*        Convertion       *)
(***************************)

(*** Exercice 7 ***)

(* Arbre binaire avec donnée dans les noeuds
type 'a arbre = Noeud of 'a arbre * 'a * 'a arbre | Vide

exception ArbreAvecCycle

(* arbre_binaire_to_graphe_ev : 'a arbre -> 'a graphe_ev *)
(* Convertis une instance d'un arbre binaire en son instance de graphe équivalente *)
(* Paramètre : l'arbre binaire à convertir *)
(* Résultat : le graphe équivalent *)
(* Exception ArbreAvecCycle, si une donnée apparait dans plusieurs noeuds de l'arbre *)
let arbre_binaire_to_graphe_ev = fun _ -> assert false

(* Pour les tests : égalité de deux grapĥes modulo permutation des listes de sommets et d'arêtes *)
let graphe_ev_eq_perm (EV (ls1, le1)) (EV (ls2, le2)) =
  eq_perm ls1 ls2 && eq_perm le1 le2

let a0 = Vide

let%test _ = graphe_ev_eq_perm (arbre_binaire_to_graphe_ev a0) g0_ev

let a1 =
  Noeud
    ( Noeud (Vide, 4, Vide),
      3,
      Noeud (Noeud (Vide, 6, Vide), 5, Noeud (Vide, 7, Vide)) )

let%test _ =
  graphe_ev_eq_perm
    (arbre_binaire_to_graphe_ev a1)
    (EV ([ 3; 4; 5; 6; 7 ], [ (3, 4); (3, 5); (5, 6); (5, 7) ]))

let a2 =
  Noeud
    ( Noeud (Vide, 4, Vide),
      3,
      Noeud (Noeud (Vide, 6, Vide), 5, Noeud (Vide, 3, Vide)) )

let%test _ =
  try
    let _ = arbre_binaire_to_graphe_ev a2 in
    false
  with ArbreAvecCycle -> true

let a3 = Noeud (Vide, 4, Vide)

let%test _ = graphe_ev_eq_perm (arbre_binaire_to_graphe_ev a3) (EV ([ 4 ], []))

let a4 = Noeud (Noeud (Vide, 6, Vide), 4, Vide)

let%test _ =
  graphe_ev_eq_perm (arbre_binaire_to_graphe_ev a4) (EV ([ 4; 6 ], [ (4, 6) ]))

let a5 = Noeud (Vide, 4, Noeud (Vide, 6, Vide))

let%test _ =
  graphe_ev_eq_perm (arbre_binaire_to_graphe_ev a4) (EV ([ 4; 6 ], [ (4, 6) ]))

let a6 = Noeud (Noeud (Vide, 2, Vide), 4, Noeud (Vide, 6, Vide))

let%test _ =
  graphe_ev_eq_perm
    (arbre_binaire_to_graphe_ev a6)
    (EV ([ 2; 4; 6 ], [ (4, 2); (4, 6) ]))

(* Arbre n-aire avec données dans les noeuds *)
type 'a arbre_naire = Node of ('a * 'a arbre_naire list)

(* arbre_naire_to_graphe_ev : 'a arbre_naire -> 'a graphe_ev *)
(* Convertis une instance d'un arbre n-aire en son instance de graphe équivalente *)
(* Paramètre : l'arbre n-aire à convertir *)
(* Résultat : le graphe équivalent *)
(* Exception ArbreAvecCycle, si une donnée apparait dans plusieurs noeuds de l'arbre *)
let arbre_naire_to_graphe_ev  = fun _ -> assert false

let an0 = Node (1, [])

let%test _ = graphe_ev_eq_perm (arbre_naire_to_graphe_ev an0) (EV ([ 1 ], []))

let an1 = Node (1, [ Node (2, []); Node (3, []); Node (4, []) ])

let%test _ =
  graphe_ev_eq_perm
    (arbre_naire_to_graphe_ev an1)
    (EV ([ 1; 2; 3; 4 ], [ (1, 2); (1, 3); (1, 4) ]))

let an2 = Node (1, [ Node (2, []); Node (1, []) ])

let%test _ =
  try
    let _ = arbre_naire_to_graphe_ev an2 in
    false
  with ArbreAvecCycle -> true

let an3 =
  Node
    ( 1,
      [
        Node (2, [ Node (21, []); Node (22, []) ]);
        Node (3, []);
        Node (4, [ Node (41, []); Node (42, []); Node (43, []); Node (44, []) ]);
      ] )

let%test _ =
  graphe_ev_eq_perm
    (arbre_naire_to_graphe_ev an3)
    (EV
       ( [ 1; 2; 3; 4; 21; 22; 41; 42; 43; 44 ],
         [
           (1, 2);
           (1, 3);
           (1, 4);
           (2, 21);
           (2, 22);
           (4, 41);
           (4, 42);
           (4, 43);
           (4, 44);
         ] ))

let an4 =
  Node
    ( 1,
      [
        Node (2, [ Node (21, [ Node (211, []) ]); Node (22, []) ]);
        Node (3, []);
        Node (4, [ Node (41, []); Node (42, []); Node (43, []); Node (44, []) ]);
      ] )

let%test _ =
  graphe_ev_eq_perm
    (arbre_naire_to_graphe_ev an4)
    (EV
       ( [ 1; 2; 3; 4; 21; 211; 22; 41; 42; 43; 44 ],
         [
           (1, 2);
           (1, 3);
           (1, 4);
           (2, 21);
           (21, 211);
           (2, 22);
           (4, 41);
           (4, 42);
           (4, 43);
           (4, 44);
         ] ))

(*** Exercice 8 ***)

(* graphe_ev_to_graphe_s : 'a graphe_ev -> 'a graphe_s *)
(* Convertis un graphe repésenté par la liste des ses somments et de ses arêtes *)
(* en un graphe représenté par la liste des couples (sommet, voisins du sommet) *)
(* Paramètre : le graphe à convertir *)
(* Précondition : le graphe est bien formé *)
(* Résultat : le graphe convertis *)
let graphe_ev_to_graphe_s  = fun _ -> assert false

(* Pour les tests : égalité de deux grapĥes modulo permutation des listes de sommets et d'arêtes *)
let graphe_s_eq_perm (Sommets ls1) (Sommets ls2) =
  List.length ls1 = List.length ls2
  && List.for_all
       (fun (s, lv) ->
         match List.assq_opt s ls1 with
         | None -> false
         | Some lv2 -> eq_perm lv lv2)
       ls2

let%test _ = graphe_s_eq_perm (graphe_ev_to_graphe_s g0_ev) g0_s
let%test _ = graphe_s_eq_perm (graphe_ev_to_graphe_s g1_ev) g1_s
let%test _ = graphe_s_eq_perm (graphe_ev_to_graphe_s g2_ev) g2_s
let%test _ = graphe_s_eq_perm (graphe_ev_to_graphe_s g3_ev) g3_s
let%test _ = graphe_s_eq_perm (graphe_ev_to_graphe_s g4_ev) g4_s

(* graphe_s_to_graphe_ev : 'a graphe_s -> 'a graphe_ev *)
(* Convertis un graphe repésenté par la liste des couples (sommet, voisins du sommet) *)
(* en un graphe représenté par la liste des ses somments et de ses arêtes *)
(* Paramètre : le graphe à convertir *)
(* Précondition : le graphe est bien formé *)
(* Résultat : le graphe convertis *)
let graphe_s_to_graphe_ev = fun _ -> assert false

let%test _ = graphe_ev_eq_perm (graphe_s_to_graphe_ev g0_s) g0_ev
let%test _ = graphe_ev_eq_perm (graphe_s_to_graphe_ev g1_s) g1_ev
let%test _ = graphe_ev_eq_perm (graphe_s_to_graphe_ev g2_s) g2_ev
let%test _ = graphe_ev_eq_perm (graphe_s_to_graphe_ev g3_s) g3_ev
let%test _ = graphe_ev_eq_perm (graphe_s_to_graphe_ev g4_s) g4_ev    *)
