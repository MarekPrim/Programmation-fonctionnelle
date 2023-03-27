(* Exercice 1*)

(* max : int list -> int  *)
(* Paramètre : liste dont on cherche le maximum *)
(* Précondition : la liste n'est pas vide *)
(* Résultat :  l'élément le plus grand de la liste *)
(* let max = fun liste -> 
  let rec aux_max max_acc reste_liste =
    match reste_liste with
    | [] -> max_acc
    | t::q -> if t > max_acc then 
                aux_max t q 
              else 
                aux_max max_acc q
  in match liste with
  | [] -> failwith "liste vide"
  | (tete::queue) -> aux_max tete queue *)


let rec max = fun liste ->
  match liste with
  | [] -> failwith ""
  | [a] -> a
  | t1::t2::q -> if t1 > t2 then max (t1::q) else max (t2::q)


  (* [(4;3);4;...] -> 4>3 -> [4;4;...] *)
let%test _ = max [ 1 ] = 1
let%test _ = max [ 1; 2 ] = 2
let%test _ = max [ 2; 1 ] = 2
let%test _ = max [ 1; 2; 3; 4; 3; 2; 1 ] = 4

(* max_max : int list list -> int  *)
(* Paramètre : la liste de listes dont on cherche le maximum *)
(* Précondition : il y a au moins un élement dans une des listes *)
(* Résultat :  l'élément le plus grand de la liste *)
let max_max = fun list_liste -> max (List.map (fun l -> max l) list_liste)

let%test _ = max_max [ [ 1 ] ] = 1
let%test _ = max_max [ [ 1 ]; [ 2 ] ] = 2
let%test _ = max_max [ [ 2 ]; [ 2 ]; [ 1; 1; 2; 1; 2 ] ] = 2
let%test _ = max_max [ [ 2 ]; [ 1 ] ] = 2
let%test _ = max_max [ [ 1; 1; 2; 1 ]; [ 1; 2; 2 ] ] = 2

let%test _ =
  max_max [ [ 1; 1; 1 ]; [ 2; 1; 2 ]; [ 3; 2; 1; 4; 2 ]; [ 1; 3; 2 ] ] = 4

(* Exercice 2*)

let rec occurence_suivante e liste =
  let rec aux_occ_sui acc reste =
    match reste with
    | [] -> (acc,reste)
    | [t] -> if t = e then (acc+1,[]) else (acc,[t])
    | t1::q -> if t1 = e then aux_occ_sui (acc+1) (q) else (acc,t1::q)
  in aux_occ_sui 0 liste

let%test _ = occurence_suivante 2 [2;2;1;2] = (2,[1;2])
let%test _ = occurence_suivante 2 [2;1;2;2] = (1,[1;2;2])
let%test _ = occurence_suivante 3 [2;1;2;2] = (0,[2;1;2;2])
(* suivant : int list -> int list *)
(* Calcule le terme suivant dans une suite de Conway *)
(* Paramètre : le terme dont on cherche le suivant *)
(* Précondition : paramètre différent de la liste vide *)
(* Retour : le terme suivant *)

let rec suivant = fun liste ->
  match liste with
  | [] -> liste
  | [_] -> 1::liste
  | (t::_) -> let (occ,reste) = occurence_suivante t liste in occ::t::(suivant reste)


let%test _ = suivant [ 1 ] = [ 1; 1 ]
let%test _ = suivant [ 2 ] = [ 1; 2 ]
let%test _ = suivant [ 3 ] = [ 1; 3 ]
let%test _ = suivant [ 1; 1 ] = [ 2; 1 ]
let%test _ = suivant [ 1; 2 ] = [ 1; 1; 1; 2 ]
let%test _ = suivant [ 1; 1; 1; 1; 3; 3; 4 ] = [ 4; 1; 2; 3; 1; 4 ]
let%test _ = suivant [ 1; 1; 1; 3; 3; 4 ] = [ 3; 1; 2; 3; 1; 4 ]
let%test _ = suivant [ 1; 3; 3; 4 ] = [ 1; 1; 2; 3; 1; 4 ]

(* suite : int -> int list -> int list list *)
(* Calcule la suite de Conway *)
(* Paramètre taille : le nombre de termes de la suite que l'on veut calculer *)
(* Paramètre depart : le terme de départ de la suite de Conway *)
(* Résultat : la suite de Conway *)
let suite  = fun taille depart ->
  let rec aux_suite acc taille reste =
    match taille with
    | 0 -> acc
    | _ -> aux_suite (acc@[reste]) (taille-1) (suivant reste)
  in aux_suite [] taille depart

let%test _ = suite 1 [ 1 ] = [ [ 1 ] ]
let%test _ = suite 2 [ 1 ] = [ [ 1 ]; [ 1; 1 ] ]
let%test _ = suite 3 [ 1 ] = [ [ 1 ]; [ 1; 1 ]; [ 2; 1 ] ]
let%test _ = suite 4 [ 1 ] = [ [ 1 ]; [ 1; 1 ]; [ 2; 1 ]; [ 1; 2; 1; 1 ] ]

let%test _ =
  suite 5 [ 1 ]
  = [ [ 1 ]; [ 1; 1 ]; [ 2; 1 ]; [ 1; 2; 1; 1 ]; [ 1; 1; 1; 2; 2; 1 ] ]

let%test _ =
  suite 10 [ 1 ]
  = [
      [ 1 ];
      [ 1; 1 ];
      [ 2; 1 ];
      [ 1; 2; 1; 1 ];
      [ 1; 1; 1; 2; 2; 1 ];
      [ 3; 1; 2; 2; 1; 1 ];
      [ 1; 3; 1; 1; 2; 2; 2; 1 ];
      [ 1; 1; 1; 3; 2; 1; 3; 2; 1; 1 ];
      [ 3; 1; 1; 3; 1; 2; 1; 1; 1; 3; 1; 2; 2; 1 ];
      [ 1; 3; 2; 1; 1; 3; 1; 1; 1; 2; 3; 1; 1; 3; 1; 1; 2; 2; 1; 1 ];
    ]

let%test _ =
  suite 10 [ 3; 3 ]
  = [
      [ 3; 3 ];
      [ 2; 3 ];
      [ 1; 2; 1; 3 ];
      [ 1; 1; 1; 2; 1; 1; 1; 3 ];
      [ 3; 1; 1; 2; 3; 1; 1; 3 ];
      [ 1; 3; 2; 1; 1; 2; 1; 3; 2; 1; 1; 3 ];
      [ 1; 1; 1; 3; 1; 2; 2; 1; 1; 2; 1; 1; 1; 3; 1; 2; 2; 1; 1; 3 ];
      [ 3; 1; 1; 3; 1; 1; 2; 2; 2; 1; 1; 2; 3; 1; 1; 3; 1; 1; 2; 2; 2; 1; 1; 3 ];
      [ 1; 3; 2; 1; 1; 3; 2; 1; 3; 2; 2; 1; 1; 2; 1; 3; 2; 1; 1; 3; 2; 1; 3; 2; 2; 1; 1; 3; ];
      [ 1; 1; 1; 3; 1; 2; 2; 1; 1; 3; 1; 2; 1; 1; 1; 3; 2; 2; 2; 1; 1; 2; 1; 1; 1; 3; 1; 2; 2; 1; 1; 3; 1; 2; 1; 1; 1; 3; 2; 2; 2; 1; 1; 3; ];
    ]

(* Tests de la conjecture *)
(* "Aucun terme de la suite, démarant à 1, ne comporte un chiffre supérieur à 3" *)
(* TO DO *)



(* Exercice 3 *)

(* Type pour représenter les expressions binaires *)
type op = Moins | Plus | Mult | Div
type exp = Binaire of exp * op * exp | Entier of int

(* eval : TO DO *)

let operation op n1 n2 = 
  match op with
  | Moins -> n1 - n2
  | Plus -> n1 + n2
  | Mult -> n1 * n2
  | Div -> n1 / n2

let rec eval arbre = 
  match arbre with
  | Entier n -> n
  | Binaire(exp1,op,exp2) -> operation op (eval exp1) (eval exp2)

let%test _ = eval (Binaire (Entier 3, Plus, Entier 4)) = 7
let%test _ = eval (Binaire (Entier 3, Plus, (Binaire (Entier 2, Mult, Entier 4)))) = 11
let%test _ = eval (Binaire (Entier 3, Plus, Entier 4)) = 7
(* Tests : TO DO *)

(* Exercice 4*)

(* Linéarisation des opérateurs binaire associatif gauche et droit *)
type exp_n = Naire of op * exp_n list | Valeur of int

(* bienformee : exp_n -> bool *)
(* Vérifie qu'un arbre n-aire représente bien une expression n-aire *)
(* c'est-à-dire que les opérateurs d'addition et multiplication ont au moins deux opérandes *)
(* et que les opérateurs de division et soustraction ont exactement deux opérandes.*)
(* Paramètre : l'arbre n-aire dont ont veut vérifier si il correspond à une expression *)
let  bienformee = 
  let rec bienformee_rec arbre = 
    match arbre with
    | Valeur _ -> true
    | Naire(op,liste) -> 
      match op with
      | Moins | Div -> 
        if List.length liste = 2 then
          List.for_all bienformee_rec liste
        else
          false
      | Plus | Mult -> 
        if List.length liste >= 2 then
          List.for_all bienformee_rec liste
        else
          false
  in bienformee_rec

(* Tests *)

let en1 = Naire (Plus, [ Valeur 3; Valeur 4; Valeur 12 ])
let en2 = Naire (Moins, [ en1; Valeur 5 ])
let en3 = Naire (Mult, [ en1; en2; en1 ])
let en4 = Naire (Div, [ en3; Valeur 2 ])
let en1err = Naire (Plus, [ Valeur 3 ])
let en2err = Naire (Moins, [ en1; Valeur 5; Valeur 4 ])
let en3err = Naire (Mult, [ en1 ])
let en4err = Naire (Div, [ en3; Valeur 2; Valeur 3 ])

let%test _ = bienformee en1
let%test _ = bienformee en2
let%test _ = bienformee en3
let%test _ = bienformee en4
let%test _ = not (bienformee en1err)
let%test _ = not (bienformee en2err)
let%test _ = not (bienformee en3err)
let%test _ = not (bienformee en4err)

(* eval : exp_n-> int *)
(* Calcule la valeur d'une expression n-aire *)
(* Paramètre : l'expression dont on veut calculer la valeur *)
(* Précondition : l'expression est bien formée *)
(* Résultat : la valeur de l'expression *)

let operation_liste op liste = 
  match op with
  | Moins -> List.fold_left (-) (List.hd liste) (List.tl liste)
  | Plus -> List.fold_left (+) (List.hd liste) (List.tl liste)
  | Mult -> List.fold_left ( * ) (List.hd liste) (List.tl liste)
  | Div -> List.fold_left (/) (List.hd liste) (List.tl liste)

let rec eval_bienformee expn =
  match expn with
  | Valeur n -> n
  | Naire (op, liste) ->
    operation_liste op (List.map eval_bienformee liste)



let%test _ = eval_bienformee en1 = 19
let%test _ = eval_bienformee en2 = 14
let%test _ = eval_bienformee en3 = 5054
let%test _ = eval_bienformee en4 = 2527

(* Définition de l'exception Malformee *)
(* TO DO *)

exception Malformee 



(* eval : exp_n-> int *)
(* Calcule la valeur d'une expression n-aire *)
(* Paramètre : l'expression dont on veut calculer la valeur *)
(* Résultat : la valeur de l'expression *)
(* Exception  Malformee si le paramètre est mal formé *)
let eval_n expn = 
    if bienformee expn then
      eval_bienformee expn
    else
      raise Malformee 

let%test _ = eval_n en1 = 19
let%test _ = eval_n en2 = 14
let%test _ = eval_n en3 = 5054
let%test _ = eval_n en4 = 2527

let%test _ =
  try
    let _ = eval_n en1err in
    false
  with Malformee -> true

let%test _ =
  try
    let _ = eval_n en2err in
    false
  with Malformee -> true

let%test _ =
  try
    let _ = eval_n en3err in
    false
  with Malformee -> true

let%test _ =
  try
    let _ = eval_n en4err in
    false
  with Malformee -> true



(* Retract : 'a list -> 'a list *)
(* Retracte une liste en supprimant les 0 inutiles*)
(* Paramètre : la liste à retracter *)
(* Résultat : la liste retractée *)

let rec retract_liste liste = 
  let rec is_rest_zero bol l =
    match l with
    | [] -> bol
    | a::q -> if a = 0 then is_rest_zero (bol && true) q else false
  in
  match liste with
  | [] -> []
  | t::q -> 
    if is_rest_zero true liste then
      []
    else
      (t)::(retract_liste q)

(* retract_liste : 'a list list -> 'a list list *)
(* Retracte une liste de liste en supprimant les 0 inutiles*)
(* Paramètre : la liste de liste à retracter *)
(* Résultat : la liste de liste retractée *)