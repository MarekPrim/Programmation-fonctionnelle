(** Exercice 1 **)

let p0 = [2.5]
let p1 = [2.;3.]
let p2 = [2.;5.5;-2.]
let p3 = [4.5;6.;-3.5;-8.]


(* evalue *)


(* evalue : float list -> float 
Prend un polynome et un x et renvoie la valeur du polynome
poly : un polynome codé avec une float list
poly : un float representant x
Retour : le resultat du polynome avec x
*)

let rec evalue poly x = match poly with 
| [] -> 0.
| hd::tl -> (evalue tl x)*.x +. hd

let%test _ = (evalue p0 0. )= 2.5
let%test _ = (evalue p0 1. )= 2.5
let%test _ = (evalue p0 (-1.)) = 2.5
let%test _ = (evalue p0 10.) = 2.5
let%test _ = (evalue p1 0. )= 2.
let%test _ = (evalue p1 1. )= 5.
let%test _ = (evalue p1 (-1.)) = -1.
let%test _ = (evalue p1 10.) = 32.
let%test _ = (evalue p2 0. )= 2.
let%test _ = (evalue p2 1. )= 5.5
let%test _ = (evalue p2 (-1.)) = -5.5
let%test _ = (evalue p2 2. )= 5.
let%test _ = (evalue p3 0. )= 4.5
let%test _ = (evalue p3 1. )= -1.
let%test _ = (evalue p3 (-1.)) = 3.
let%test _ = (evalue p3 2. )= -61.5

(* retract : float list -> float list 
Prend un polynome et supprime les coéfficients nuls inutiles (ceux de la fin de la liste)
poly : un polynome codé avec une float list
Retour : équivalent à poly sans zéro superflu
*)

let rec retract poly =
  match poly with
  | []->[]
  | h::t -> 
    let prev = retract t in
    if h = 0.0 && prev = [] 
      then []
      else h::prev

let%test _ = retract p0 = p0
let%test _ = retract p1 = p1
let%test _ = retract p2 = p2
let%test _ = retract p3 = p3
let%test _ = retract [2.;0.;0.;0.;0.;0.] = [2.]
let%test _ = retract [2.;0.;0.;0.;0.;2.] = [2.;0.;0.;0.;0.;2.]
let%test _ = retract [2.;0.;0.;0.;0.;2.;0.;0.] = [2.;0.;0.;0.;0.;2.]
let%test _ = retract [2.;0.;0.;0.;0.;2.;0.] = [2.;0.;0.;0.;0.;2.]
let%test _ = retract [2.;3.;0.;0.;0.;0.] = [2.;3.]
let%test _ = retract [2.;5.;-2.;0.;0.;0.] = [2.;5.;-2.]
let%test _ = retract [4.;6.;-3.;-8.;0.;0.] = [4.;6.;-3.;-8.]


(* scal_mult : float -> float liste -> float list 
Fonction qui multiplie un polynome par un scalaire
Parametre x : un réel
Paramètre poly : un polynome codé avec une float list
Résultat : x * poly, un polynome
*)

let scal_mult x = if x = 0. then (fun _ -> []) else List.map (fun a -> a*.x) 

let%test _ = scal_mult 0. p0 = []
let%test _ = scal_mult 0. p1 = []
let%test _ = scal_mult 0. p2 = []
let%test _ = scal_mult 0. p3 = []
let%test _ = scal_mult 1. p0 = p0
let%test _ = scal_mult 1. p1 = p1
let%test _ = scal_mult 1. p2 = p2
let%test _ = scal_mult 1. p3 = p3
let%test _ = scal_mult 2. p0 = [5.]
let%test _ = scal_mult 2. p1 = [4.;6.]
let%test _ = scal_mult 2. p2 = [4.;11.;-4.]
let%test _ = scal_mult 2. p3 = [9.;12.;-7.;-16.]

(* plus : float list -> float list -> float list
Fonction qui fait la somme de deux polynomes
Paramètre poly1, poly2 : deux polynomes codés avec une float list
Resultat : un polynome, somme de poly1 et poly2
*)

let plus poly1 poly2 = 
  let rec aux poly1 poly2 =
    match poly1 with 
    | [] -> poly2
    | h1::t1 -> 
      match poly2 with 
      | [] -> poly1
      | h2::t2 -> (h1+.h2)::(aux t1 t2)
  in retract (aux poly1 poly2)


let%test _ = plus p0 p0 = [5.]
let%test _ = plus p0 p1 = [4.5;3.]
let%test _ = plus p0 p2 = [4.5;5.5;-2.]
let%test _ = plus p0 p3 = [7.;6.;-3.5;-8.]
let%test _ = plus p1 p0 = plus p0 p1
let%test _ = plus p1 p1 = [4.;6.]
let%test _ = plus p1 p2 = [4.;8.5;-2.]
let%test _ = plus p1 p3 = [6.5;9.;-3.5;-8.]
let%test _ = plus p2 p0 = plus p0 p2
let%test _ = plus p2 p1 = plus p1 p2
let%test _ = plus p2 p2 = [4.;11.;-4.]
let%test _ = plus p2 p3 = [6.5;11.5;-5.5;-8.]
let%test _ = plus p3 p0 = plus p0 p3
let%test _ = plus p3 p1 = plus p1 p3
let%test _ = plus p3 p2 = plus p2 p3
let%test _ = plus p3 p3 = [9.;12.;-7.;-16.]
let%test _ = plus p3 [-4.5;-6.;3.5;8.] = []
let%test _ = plus p3 [-4.5;-6.;3.5] = [0.;0.;0.;-8.]
let%test _ = plus p3 [0.;-6.;3.5;8.] = [4.5]
let%test _ = plus p3 [0.;0.;3.5;8.] = [4.5;6.]
let%test _ = plus p3 [0.;0.;0.;8.] = [4.5;6.;-3.5]


(* Exercice 2 *)

open Util

(* Arbre d'encodage : arbre ternaire *)
type arbre_encodage = Vide | Lettre of char | Noeud of arbre_encodage * arbre_encodage * arbre_encodage

(* Arbre d'encodage complexe du sujet 
Permet de coder les mots sur {'a','b','c','d','e','f'} 
en codant le 'a' par 3, le 'b' par 12, le 'c' par 212, le 'd' par 22, le 'e' par 11 et le 'f' par 213
*)
let arbre_sujet =
  Noeud (
          Noeud (Lettre 'e',Lettre 'b',Vide) ,
          Noeud (
                Noeud (Vide, Lettre 'c', Lettre 'f'),
                Lettre 'd',
                Vide),
          Lettre 'a'
        )

(* Arbre d'encodage simple du sujet
Permet de coder les mots sur {'a','b','c'} 
en codant le 'a' par 1, le 'b' par 2 et le 'c' par 3*)
let arbre_simple =
  Noeud (Lettre 'a', Lettre 'b', Lettre 'c')


(* Exception levée quand le code ne peut pas être décodé avec l'arbre d'encodage *)
exception CodeNonValide

(* decoder : int -> arbre_encodage -> string
Décode un entier en utilisant l'abre d'encodage
Paramètre code : l'entier à décoder
Paramètre arbre : l'arbre d'encodage
Retour : la chaîne de caractère correspondant à l'entier
Erreur si le code ne peut pas être décodé avec l'arbre d'encodage
*)
let decoder code racine = 
  let rec aux code arbre = 
    match code with 
    |[]-> (
      match arbre with 
      |Lettre(c) -> [c]
      |_ -> if arbre = racine then [] else raise CodeNonValide
      )
    |hd::tl-> (
      match arbre with 
      | Lettre(c) -> c::(aux code racine)
      | Vide -> raise CodeNonValide
      | Noeud(a1,a2,a3) -> (
        match hd with 
        |1-> aux tl a1
        |2-> aux tl a2
        |3-> aux tl a3
        |_-> raise CodeNonValide
        )
      )
  in recompose_chaine (aux (decompose_int code) racine) 

let%test _ = decoder 123212 arbre_simple = "abcbab"
let%test _ = decoder 123212 arbre_sujet = "bac"
let%test _ = decoder 123 arbre_simple = "abc"
let%test _ = decoder 123 arbre_sujet = "ba"
let%test _ = decoder 321321 arbre_simple = "cbacba"
let%test _ = try let _ = decoder 321321 arbre_sujet in false with CodeNonValide -> true
let%test _ = try let _ = decoder 457 arbre_simple in false with CodeNonValide -> true

(* arbre_to_liste : arbre_encodage -> (char*int) list
  Converti un arbre d'encodage en une liste associative (caractère, code)
  Paramètre a : l'abre à convertir
  Retour la liste  associative (caractère, code)
*)
let arbre_to_liste arbre =
  let appendcode i (c,l) = (c,i::l) in 
  let rec aux arbre = 
    match arbre with
    |Lettre(c) -> [(c, [])]
    |Vide -> []
    |Noeud(a1, a2, a3) -> 
      (List.map (appendcode 1) (aux a1)) @
      (List.map (appendcode 2) (aux a2)) @
      (List.map (appendcode 3) (aux a3))
  in 
    List.map (fun (c, l) -> (c, recompose_int l)) (aux arbre)
      

let liste_arbre_simple = arbre_to_liste arbre_simple
let%test _ = List.length liste_arbre_simple =3
let%test _ = List.mem ('a',1) liste_arbre_simple
let%test _ = List.mem ('b',2) liste_arbre_simple
let%test _ = List.mem ('c',3) liste_arbre_simple

let liste_arbre_sujet = arbre_to_liste arbre_sujet
let%test _ = List.length liste_arbre_sujet =6
let%test _ = List.mem ('a',3) liste_arbre_sujet
let%test _ = List.mem ('b',12) liste_arbre_sujet
let%test _ = List.mem ('c',212) liste_arbre_sujet
let%test _ = List.mem ('d',22) liste_arbre_sujet
let%test _ = List.mem ('e',11) liste_arbre_sujet
let%test _ = List.mem ('f',213) liste_arbre_sujet

(* Exception levée quand le mot ne peut pas être encodé avec l'arbre d'encodage *)
exception MotNonValide


(* encoder : string -> arbre_encodage -> int
Encode un mot à l'aide d'un arbre d'encodage
Paramètre mot : le mot à encoder
Paramètre arbre : l'arbre d'encodage
Retour : le code associé au mot
Erreur si le mot ne peut pas être encodé avec l'arbre d'encodage
*)
let encoder mot arbre = 
  let codes = arbre_to_liste arbre in
  let encode_character c = 
    match (List.find_opt (fun (x, _) -> x=c) codes) with
    | Some((_, l)) -> decompose_int l
    | None -> raise MotNonValide
  in 
  recompose_int (List.fold_right (fun  c acc -> (encode_character c ) @ acc)  (decompose_chaine mot) []) 

let%test _ = encoder "abcbab" arbre_simple = 123212
let%test _ = encoder "bac" arbre_sujet = 123212
let%test _ = encoder "abc" arbre_simple = 123
let%test _ = encoder "ba" arbre_sujet = 123
let%test _ = encoder "cbacba" arbre_simple = 321321
let%test _ = try let _ = encoder "dab" arbre_simple in false with MotNonValide -> true 
let%test _ = try let _ = encoder "zut" arbre_simple in false with MotNonValide -> true 


(* fold :  COMPLETER
Itérateur fold pour les arbres d'encodage
Paramètre f: la fontion 'a -> arbre -> 'a
Paramètre arbre: l'arbre d'encodage
Retour : l'accumulateur
*)
let rec fold f fe arbre = 
  match arbre with
  | Vide -> fe None
  | Lettre(c) -> fe (Some(c))
  | Noeud (a1, a2, a3) -> 
    f (fold f fe a1) (fold f fe a2) (fold f fe a3)

(* arbre_encodage : arbre_encodage -> int
Fonction qui calcule le nombre de lettres présentent dans un arbre d'encodage
Paramètre : l'arbre d'encodage
Retour : le nombre de lettre
*)

let nbLettres arbre = 
  fold 
    (fun a1 a2 a3 -> a1 + a2 + a3) 
    (fun e -> 
      match e with
      |Some(_) -> 1 
      |None -> 0) 
    arbre

let%test _ = nbLettres arbre_simple = 3
let%test _ = nbLettres arbre_sujet = 6


(* lettres :  arbre_encodage -> char list
Fonction qui renvoie la liste des lettres présentent dans un arbre d'encodage
Paramètre : l'arbre d'encodage
Retour : la liste de lettre
*)
let lettres = 
  fold 
    (fun a1 a2 a3 -> a1 @ a2 @ a3) 
    (fun e -> 
      match e with
      |Some(c) -> [c] 
      |None -> [])  

let lettres_arbre_simple = lettres arbre_simple
let%test _ = List.length lettres_arbre_simple = 3
let%test _ = List.mem 'a' lettres_arbre_simple
let%test _ = List.mem 'b' lettres_arbre_simple
let%test _ = List.mem 'c' lettres_arbre_simple

let lettres_arbre_sujet = lettres arbre_sujet
let%test _ = List.length lettres_arbre_sujet = 6
let%test _ = List.mem 'a' lettres_arbre_sujet
let%test _ = List.mem 'b' lettres_arbre_sujet
let%test _ = List.mem 'c' lettres_arbre_sujet
let%test _ = List.mem 'd' lettres_arbre_sujet
let%test _ = List.mem 'e' lettres_arbre_sujet
let%test _ = List.mem 'f' lettres_arbre_sujet


(* arbre_to_liste_2 : arbre_encodage -> (char*int) list
  Converti un arbre d'encodage en une liste associative (caractère, code)
  Paramètre a : l'abre à convertir
  Retour la liste  associative (caractère, code)
*)
let arbre_to_liste_2 arbre = 
  let appendcode i (c,l) = (c,i::l) in 
  let handle_tip tip = 
    match tip with
    | Some(c) -> [(c, [])]
    | None -> []
  in 
  let handle_node a1 a2 a3 = 
    (List.map (appendcode 1) a1) @
    (List.map (appendcode 2) a2) @
    (List.map (appendcode 3) a3)
  in 
  List.map (fun (c, l) -> (c,recompose_int l)) (fold handle_node handle_tip arbre)



let liste_arbre_simple_2 = arbre_to_liste_2 arbre_simple
let%test _ = List.length liste_arbre_simple_2 =3
let%test _ = List.mem ('a',1) liste_arbre_simple_2
let%test _ = List.mem ('b',2) liste_arbre_simple_2
let%test _ = List.mem ('c',3) liste_arbre_simple_2

let liste_arbre_sujet_2 = arbre_to_liste_2 arbre_sujet
let%test _ = List.length liste_arbre_sujet_2 =6
let%test _ = List.mem ('a',3) liste_arbre_sujet_2
let%test _ = List.mem ('b',12) liste_arbre_sujet_2
let%test _ = List.mem ('c',212) liste_arbre_sujet_2
let%test _ = List.mem ('d',22) liste_arbre_sujet_2
let%test _ = List.mem ('e',11) liste_arbre_sujet_2
let%test _ = List.mem ('f',213) liste_arbre_sujet_2
