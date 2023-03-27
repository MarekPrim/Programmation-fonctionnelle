(****************************)
(** Expressions régulières **)
(****************************)

(* Type des expressions régulières sur l'alphabet 'a *)
type 'a expReg =
  | Symbole of 'a (* Symbole de l'alphabet *)
  | Lambda (* Mot vide *)
  | Vide (* Ensemble vide *)
  | Ou of 'a expReg * 'a expReg (* exp + exp *)
  | Suite of 'a expReg * 'a expReg (* exp.exp *)
  | Etoile of 'a expReg
(*exp * *)

(* Définition des expressions régulières du sujet pour les tests*)
(* (a+b)*  *)
let expReg1 = Etoile (Ou (Symbole 'a', Symbole 'b'))

(* a.(b+c).a* *)
let expReg2 =
  Suite
    (Suite (Symbole 'a', Ou (Symbole 'b', Symbole 'c')), Etoile (Symbole 'a'))

let expReg2Bis =
  Suite
    (Symbole 'a', Suite (Ou (Symbole 'b', Symbole 'c'), Etoile (Symbole 'a')))

let expReg3 = Ou (Etoile (Symbole 1), Etoile (Symbole 0))

(* CONTRAT : TO DO *)
(* mot_vide_dans : 'a expReg -> boolean*)
(* Indique si le mot vide est présent dans l'expression régulière *)
(* regexp : L'expression dont on souhaite connaitre la présence du mot vide *)
let rec mot_vide_dans  regexp =
  match regexp with
  | Symbole _ -> false
  | Etoile (_) -> true
  | Lambda -> true
  | Vide -> false
  | Suite (exp1,exp2) -> (mot_vide_dans exp1) && (mot_vide_dans exp2)
  | Ou (exp1,exp2) -> (mot_vide_dans exp1) || (mot_vide_dans exp2)
  
  
(* TESTS : TO DO *)

let%test _ = mot_vide_dans expReg1 = true
let%test _ = mot_vide_dans expReg2 = false

let%test _ = mot_vide_dans expReg2Bis = false
let%test _ = mot_vide_dans (Lambda) = true
let%test _ = mot_vide_dans (Vide) = false
let%test _ = mot_vide_dans (Suite((Symbole 'a'),(Symbole 'a'))) = false


(* derive : 'a -> 'a expReg -> 'a expReg *)
(* Calcule la dérivée par rapport à a de l'exp e *)
(* a : le symbole du langage par rapport auquel est dérivée l'expression *)
(* e : l'expression à dériver *)
(* La simplification de l'expression obtenue n'est pas demandée *)

(* delta : 'a expReg -> 'b expReg *)
(* Calcule le delta d'une expression *)
(* langage : Expression dont il faut déterminer le delta *)
let delta langage =
  if mot_vide_dans langage then
    Lambda
  else
    Vide


let%test _ = delta (Symbole 't') = Vide
let%test _ = delta (Etoile(Symbole 't')) = Lambda
(* a*.( r+c* ) contient le mot vide*)
let%test _ = delta (Suite(Etoile(Symbole 'a'),Ou(Symbole 'r',Etoile(Symbole 'c')))) = Lambda 


let rec derive symbol regexp =
  match regexp with
  | Lambda -> Vide
  | Vide -> Vide
  | Symbole a-> if a=symbol then Lambda else Vide
  | Suite(exp1,exp2) -> Ou(Suite(derive symbol exp1,exp2),Suite(delta exp1,derive symbol exp2))
  | Ou (exp1,exp2) -> Ou(derive symbol exp1,derive symbol exp2)
  | Etoile (exp) -> Suite(derive symbol exp,Etoile(exp))


(* Les tests ne passeront que si AUCUNE simplification de l'expression obtenue n'est faite *)
(* Si vous faites des simplifications, écrivez vos propres tests *)

let%test _ = derive 'a' Lambda = Vide
let%test _ = derive 'a' Vide = Vide
let%test _ = derive 'a' (Symbole 'b') = Vide
let%test _ = derive 'a' (Symbole 'a') = Lambda
let%test _ = derive 'a' (Ou (Symbole 'a', Symbole 'b')) = Ou (Lambda, Vide)

let%test _ =
  derive 'a' (Suite (Symbole 'a', Symbole 'b'))
  = Ou (Suite (Lambda, Symbole 'b'), Suite (Vide, Vide))

let%test _ =
  derive 'a' (Suite (Symbole 'b', Symbole 'a'))
  = Ou (Suite (Vide, Symbole 'a'), Suite (Vide, Lambda))

let%test _ =
  derive 'a' (Etoile (Symbole 'a')) = Suite (Lambda, Etoile (Symbole 'a'))

let%test _ =
  derive 'a' (Etoile (Symbole 'b')) = Suite (Vide, Etoile (Symbole 'b'))

let%test _ = derive 'a' expReg1 = Suite (Ou (Lambda, Vide), expReg1)
let%test _ = derive 'b' expReg1 = Suite (Ou (Vide, Lambda), expReg1)

let%test _ =
  derive 'a' (Suite (Symbole 'a', Ou (Symbole 'b', Symbole 'c')))
  = Ou
      ( Suite (Lambda, Ou (Symbole 'b', Symbole 'c')),
        Suite (Vide, Ou (Vide, Vide)) )

let%test _ =
  derive 'a' expReg2
  = Ou
      ( Suite
          ( Ou
              ( Suite (Lambda, Ou (Symbole 'b', Symbole 'c')),
                Suite (Vide, Ou (Vide, Vide)) ),
            Etoile (Symbole 'a') ),
        Suite (Vide, Suite (Lambda, Etoile (Symbole 'a'))) )

let%test _ =
  derive 1 expReg3
  = Ou (Suite (Lambda, Etoile (Symbole 1)), Suite (Vide, Etoile (Symbole 0)))

let%test _ =
  derive 0 expReg3
  = Ou (Suite (Vide, Etoile (Symbole 1)), Suite (Lambda, Etoile (Symbole 0)))


(* appartient_langage_expReg : 'a expReg -> 'a list -> bool *)
(* Teste si un mot appartient au langage associé à l'expression régulière *)
(* let rec appartient_langage_expReg regexp mot =
  match mot with
  | [] -> mot_vide_dans regexp
  | t::q ->
      appartient_langage_expReg (derive t regexp) q *)

(* Pour déterminer l'appartenance de mot à la regexp, il faut vérifier que le mot vide appartient *)
(* à la regexp issue de la dérivation récursive de la regexp d'origine pour chacun des charactères de mot *)
(* a • m ∈ L(e) ssi m ∈ L(Da(e)) ==> a•Λ ∈ L(e) ssi Λ ∈ L(Da(Db(Dc(......e))))*)
let appartient_langage_expReg regexp mot =
  List.fold_right (fun tete queue_traitee -> mot_vide_dans (derive tete regexp) || queue_traitee ) mot (mot_vide_dans regexp) 

  (* let appartient_langage_expReg regexp mot =
    List.fold_left (fun acc last -> mot_vide_dans (derive last regexp) || acc )  (mot_vide_dans regexp) mot *)


let%test _ = appartient_langage_expReg expReg1 []
let%test _ = appartient_langage_expReg expReg1 [ 'a' ]
let%test _ = appartient_langage_expReg expReg1 [ 'b' ]
let%test _ = appartient_langage_expReg expReg1 [ 'a'; 'a' ]
let%test _ = appartient_langage_expReg expReg1 [ 'a'; 'b' ]
let%test _ = appartient_langage_expReg expReg1 [ 'b'; 'a'; 'b'; 'b' ]
let%test _ = not (appartient_langage_expReg expReg1 [ 'c' ])
let%test _ = not (appartient_langage_expReg expReg1 [ 'c'; 'a'; 'b' ])
let%test _ = not (appartient_langage_expReg expReg1 [ 'a'; 'a'; 'c' ])
let%test _ = not (appartient_langage_expReg expReg1 [ 'a'; 'c'; 'b' ])
let%test _ = not (appartient_langage_expReg expReg1 [ 'c' ])
let%test _ = not (appartient_langage_expReg expReg1 [ 'c'; 'a'; 'b' ])
let%test _ = not (appartient_langage_expReg expReg1 [ 'a'; 'a'; 'c' ])
let%test _ = not (appartient_langage_expReg expReg1 [ 'a'; 'c'; 'b' ])
let%test _ = not (appartient_langage_expReg expReg1 [ 'd' ])
let%test _ = appartient_langage_expReg expReg2 [ 'a'; 'b' ]
let%test _ = appartient_langage_expReg expReg2 [ 'a'; 'c' ]
let%test _ = appartient_langage_expReg expReg2 [ 'a'; 'b'; 'a' ]
let%test _ = appartient_langage_expReg expReg2 [ 'a'; 'c'; 'a'; 'a' ]
let%test _ = appartient_langage_expReg expReg2 [ 'a'; 'b'; 'a'; 'a'; 'a' ]
let%test _ = appartient_langage_expReg expReg2 [ 'a'; 'c'; 'a'; 'a'; 'a'; 'a' ]
let%test _ = appartient_langage_expReg expReg2Bis [ 'a'; 'b' ]
let%test _ = appartient_langage_expReg expReg2Bis [ 'a'; 'c' ]
let%test _ = appartient_langage_expReg expReg2Bis [ 'a'; 'b'; 'a' ]
let%test _ = appartient_langage_expReg expReg2Bis [ 'a'; 'c'; 'a'; 'a' ]
let%test _ = appartient_langage_expReg expReg2Bis [ 'a'; 'b'; 'a'; 'a'; 'a' ]

let%test _ =
  appartient_langage_expReg expReg2Bis [ 'a'; 'c'; 'a'; 'a'; 'a'; 'a' ]

let%test _ = not (appartient_langage_expReg expReg2 [ 'a'; 'b'; 'b' ])
let%test _ = not (appartient_langage_expReg expReg2 [ 'a'; 'b'; 'c' ])
let%test _ = not (appartient_langage_expReg expReg2 [ 'a'; 'c'; 'b' ])
let%test _ = not (appartient_langage_expReg expReg2 [ 'a'; 'c'; 'c' ])
let%test _ = not (appartient_langage_expReg expReg2 [ 'c'; 'b' ])
let%test _ = not (appartient_langage_expReg expReg2 [ 'c'; 'c' ])
let%test _ = not (appartient_langage_expReg expReg2 [ 'c'; 'b'; 'a' ])
let%test _ = not (appartient_langage_expReg expReg2 [ 'c'; 'c'; 'a'; 'a' ])
let%test _ = not (appartient_langage_expReg expReg2 [ 'c'; 'b'; 'a'; 'a'; 'a' ])

let%test _ =
  not (appartient_langage_expReg expReg2 [ 'c'; 'c'; 'a'; 'a'; 'a'; 'a' ])

let%test _ = not (appartient_langage_expReg expReg2 [ 'b'; 'b' ])
let%test _ = not (appartient_langage_expReg expReg2 [ 'b'; 'c' ])
let%test _ = not (appartient_langage_expReg expReg2 [ 'b'; 'b'; 'a' ])
let%test _ = not (appartient_langage_expReg expReg2 [ 'b'; 'c'; 'a'; 'a' ])
let%test _ = not (appartient_langage_expReg expReg2 [ 'b'; 'b'; 'a'; 'a'; 'a' ])
let%test _ = not (appartient_langage_expReg expReg2 [ 'd' ])

let%test _ =
  not (appartient_langage_expReg expReg2 [ 'b'; 'c'; 'a'; 'a'; 'a'; 'a' ])

let%test _ = not (appartient_langage_expReg expReg2Bis [ 'a'; 'b'; 'b' ])
let%test _ = not (appartient_langage_expReg expReg2Bis [ 'a'; 'b'; 'c' ])
let%test _ = not (appartient_langage_expReg expReg2Bis [ 'a'; 'c'; 'b' ])
let%test _ = not (appartient_langage_expReg expReg2Bis [ 'a'; 'c'; 'c' ])
let%test _ = not (appartient_langage_expReg expReg2Bis [ 'c'; 'b' ])
let%test _ = not (appartient_langage_expReg expReg2Bis [ 'c'; 'c' ])
let%test _ = not (appartient_langage_expReg expReg2Bis [ 'c'; 'b'; 'a' ])
let%test _ = not (appartient_langage_expReg expReg2Bis [ 'c'; 'c'; 'a'; 'a' ])

let%test _ =
  not (appartient_langage_expReg expReg2Bis [ 'c'; 'b'; 'a'; 'a'; 'a' ])

let%test _ =
  not (appartient_langage_expReg expReg2Bis [ 'c'; 'c'; 'a'; 'a'; 'a'; 'a' ])

let%test _ = not (appartient_langage_expReg expReg2Bis [ 'b'; 'b' ])
let%test _ = not (appartient_langage_expReg expReg2Bis [ 'b'; 'c' ])
let%test _ = not (appartient_langage_expReg expReg2Bis [ 'b'; 'b'; 'a' ])
let%test _ = not (appartient_langage_expReg expReg2Bis [ 'b'; 'c'; 'a'; 'a' ])

let%test _ =
  not (appartient_langage_expReg expReg2Bis [ 'b'; 'b'; 'a'; 'a'; 'a' ])

let%test _ =
  not (appartient_langage_expReg expReg2Bis [ 'b'; 'c'; 'a'; 'a'; 'a'; 'a' ])

let%test _ = not (appartient_langage_expReg expReg2Bis [ 'd' ])
let%test _ = appartient_langage_expReg expReg3 []
let%test _ = appartient_langage_expReg expReg3 [ 1 ]
let%test _ = appartient_langage_expReg expReg3 [ 1; 1 ]
let%test _ = appartient_langage_expReg expReg3 [ 1; 1; 1 ]
let%test _ = appartient_langage_expReg expReg3 [ 1; 1; 1; 1 ]
let%test _ = appartient_langage_expReg expReg3 [ 0 ]
let%test _ = appartient_langage_expReg expReg3 [ 0; 0 ]
let%test _ = appartient_langage_expReg expReg3 [ 0; 0; 0 ]
let%test _ = appartient_langage_expReg expReg3 [ 0; 0; 0; 0 ]
let%test _ = not (appartient_langage_expReg expReg3 [ 1; 0 ])
let%test _ = not (appartient_langage_expReg expReg3 [ 2 ])
let%test _ = not (appartient_langage_expReg expReg3 [ 0; 1 ])
let%test _ = not (appartient_langage_expReg expReg3 [ 1; 0; 1 ])
let%test _ = not (appartient_langage_expReg expReg3 [ 1; 0; 0 ])
let%test _ = not (appartient_langage_expReg expReg3 [ 0; 1; 1 ])
let%test _ = not (appartient_langage_expReg expReg3 [ 0; 1; 0 ])


(*********************************)
(** Automates déterministes **)
(*********************************)

(* Un automate fini non déterministe est un quintuplet *)
type ('a, 'b) afd =
  | AFD of
      'b list
      (* ensemble fini d'états *)
      * 'a list
      (* alphabet *)
      * ('b * 'a * 'b) list
      (* transitions de l'automate *)
      * 'b
      (* état initial de l'automate*)
      * 'b list
(* états finaux de l'automate *)

(** Automates du sujet pour des tests **)
let afd1 =
  AFD
    ([ 0; 1 ], [ 'a'; 'b' ], [ (0, 'a', 1); (1, 'a', 0); (1, 'b', 1) ], 0, [ 1 ])

let afd2 =
  AFD
    ( [ "q0"; "q1"; "q2" ],
      [ 0; 1 ],
      [ ("q0", 1, "q1"); ("q1", 0, "q1"); ("q1", 1, "q2") ],
      "q0",
      [ "q1"; "q2" ] )

let afd3 =
  AFD
    ( [ "q0"; "q1"; "q2"; "q3"; "q4"; "q5"; "q6"; "q7" ],
      [ 'a'; 'b'; 'c' ],
      [
        ("q0", 'a', "q1");
        ("q0", 'b', "q3");
        ("q1", 'c', "q2");
        ("q1", 'b', "q4");
        ("q2", 'c', "q1");
        ("q2", 'b', "q5");
        ("q3", 'a', "q4");
        ("q3", 'b', "q6");
        ("q4", 'b', "q5");
        ("q4", 'c', "q6");
        ("q5", 'a', "q7");
        ("q5", 'b', "q5");
        ("q6", 'a', "q6");
        ("q6", 'c', "q4");
        ("q7", 'a', "q6");
        ("q7", 'b', "q4");
      ],
      "q0",
      [ "q0"; "q2"; "q6"; "q7" ] )

(* Pour les tests *)
(* [eq_perm l l'] retourne true ssi [l] et [l']
   sont égales à à permutation près (pour (=)).
   [l'] ne doit pas contenir de doublon. *)
let eq_perm l l' =
  List.length l = List.length l' && List.for_all (fun x -> List.mem x l) l'

(* init_finaux_bienformees : ('a, 'b) afd -> bool *)
(* Vérifie qui les états initiaux et les états terminaux *)
(* sont bien déclarés comme états de l'automates. *)
let init_finaux_bienformees  (AFD(etats,_,_,etat_initial,etat_finaux)) =
  (List.for_all (fun ef -> List.mem ef etats) etat_finaux) && (List.mem etat_initial etats)



let%test _ = init_finaux_bienformees afd1
let%test _ = init_finaux_bienformees afd2
let%test _ = init_finaux_bienformees afd3

let%test _ =
  not
    (init_finaux_bienformees
       (AFD
          ( [ 0; 1 ],
            [ 'a'; 'b' ],
            [ (0, 'a', 1); (1, 'a', 0); (1, 'b', 1) ],
            3,
            [ 1 ] )))

let%test _ =
  not
    (init_finaux_bienformees
       (AFD
          ( [ 0; 1 ],
            [ 'a'; 'b' ],
            [ (0, 'a', 1); (1, 'a', 0); (1, 'b', 1) ],
            0,
            [ 3 ] )))

let%test _ =
  not
    (init_finaux_bienformees
       (AFD
          ( [ 0; 1 ],
            [ 'a'; 'b' ],
            [ (0, 'a', 1); (1, 'a', 0); (1, 'b', 1) ],
            0,
            [ 1; 3; 0 ] )))


(* transitions_bienformees :  ('a, 'b) afd -> bool *)
(* Vérifie que les transitions sont bien formée, c'est à dire que *)
(* les états de départ et d'arrivée des transitions sont bien déclarés *)
(* comme états de l'automates et que les symboles des transitions sont *)
(* bien dans l'alphabet de l'automates *)
let transitions_bienformees  (AFD(etats,alphabet,transitions,_,_)) =
  List.for_all ( fun (src,symb,tgt) -> List.mem src etats && List.mem tgt etats && List.mem symb alphabet) transitions


let%test _ = transitions_bienformees afd1
let%test _ = transitions_bienformees afd2
let%test _ = transitions_bienformees afd3

let%test _ =
  not
    (transitions_bienformees
       (AFD
          ( [ 0; 1 ],
            [ 'a'; 'b' ],
            [ (3, 'a', 1); (1, 'a', 0); (1, 'b', 1) ],
            0,
            [ 1 ] )))

let%test _ =
  not
    (transitions_bienformees
       (AFD
          ( [ 0; 1 ],
            [ 'a'; 'b' ],
            [ (0, 'a', 3); (1, 'a', 0); (1, 'b', 1) ],
            0,
            [ 1 ] )))

let%test _ =
  not
    (transitions_bienformees
       (AFD
          ( [ 0; 1 ],
            [ 'a'; 'b' ],
            [ (0, 'a', 1); (1, 'a', 3); (1, 'b', 1) ],
            0,
            [ 1 ] )))

let%test _ =
  not
    (transitions_bienformees
       (AFD
          ( [ 0; 1 ],
            [ 'a'; 'b' ],
            [ (0, 'a', 1); (3, 'a', 0); (1, 'b', 1) ],
            0,
            [ 1 ] )))

let%test _ =
  not
    (transitions_bienformees
       (AFD
          ( [ 0; 1 ],
            [ 'a'; 'b' ],
            [ (0, 'c', 1); (1, 'a', 0); (1, 'b', 1) ],
            0,
            [ 1 ] )))

let%test _ =
  not
    (transitions_bienformees
       (AFD
          ( [ 0; 1 ],
            [ 'a'; 'b' ],
            [ (0, 'a', 1); (1, 'c', 0); (1, 'b', 1) ],
            0,
            [ 1 ] )))


(* get_destinations : ('a,'b) afnd -> 'b -> 'a -> 'b list *)
(* Calcule la liste de états étteignable depuis [etat] par la transition [trans] *)
let get_destinations (AFD(_,_,tr,_,_)) start_state symbole_transition =
  List.filter_map (fun (src,syml_tr,tgt) -> if (src=start_state && syml_tr=symbole_transition) then Some(tgt) else None) tr


let%test _ = eq_perm (get_destinations afd1 0 'a') [ 1 ]
let%test _ = eq_perm (get_destinations afd1 0 'b') []
let%test _ = eq_perm (get_destinations afd1 1 'a') [ 0 ]
let%test _ = eq_perm (get_destinations afd1 1 'b') [ 1 ]
let%test _ = eq_perm (get_destinations afd2 "q0" 0) []
let%test _ = eq_perm (get_destinations afd2 "q0" 1) [ "q1" ]
let%test _ = eq_perm (get_destinations afd2 "q1" 0) [ "q1" ]
let%test _ = eq_perm (get_destinations afd2 "q1" 1) [ "q2" ]
let%test _ = eq_perm (get_destinations afd2 "q2" 0) []
let%test _ = eq_perm (get_destinations afd2 "q2" 1) []
let%test _ = eq_perm (get_destinations afd3 "q0" 'a') [ "q1" ]
let%test _ = eq_perm (get_destinations afd3 "q0" 'b') [ "q3" ]
let%test _ = eq_perm (get_destinations afd3 "q0" 'c') []
let%test _ = eq_perm (get_destinations afd3 "q1" 'a') []
let%test _ = eq_perm (get_destinations afd3 "q1" 'b') [ "q4" ]
let%test _ = eq_perm (get_destinations afd3 "q1" 'c') [ "q2" ]
let%test _ = eq_perm (get_destinations afd3 "q2" 'a') []
let%test _ = eq_perm (get_destinations afd3 "q2" 'b') [ "q5" ]
let%test _ = eq_perm (get_destinations afd3 "q2" 'c') [ "q1" ]
let%test _ = eq_perm (get_destinations afd3 "q3" 'a') [ "q4" ]
let%test _ = eq_perm (get_destinations afd3 "q3" 'b') [ "q6" ]
let%test _ = eq_perm (get_destinations afd3 "q3" 'c') []
let%test _ = eq_perm (get_destinations afd3 "q4" 'a') []
let%test _ = eq_perm (get_destinations afd3 "q4" 'b') [ "q5" ]
let%test _ = eq_perm (get_destinations afd3 "q4" 'c') [ "q6" ]
let%test _ = eq_perm (get_destinations afd3 "q5" 'a') [ "q7" ]
let%test _ = eq_perm (get_destinations afd3 "q5" 'b') [ "q5" ]
let%test _ = eq_perm (get_destinations afd3 "q5" 'c') []
let%test _ = eq_perm (get_destinations afd3 "q6" 'a') [ "q6" ]
let%test _ = eq_perm (get_destinations afd3 "q6" 'b') []
let%test _ = eq_perm (get_destinations afd3 "q6" 'c') [ "q4" ]
let%test _ = eq_perm (get_destinations afd3 "q7" 'a') [ "q6" ]
let%test _ = eq_perm (get_destinations afd3 "q7" 'b') [ "q4" ]
let%test _ = eq_perm (get_destinations afd3 "q7" 'c') []

let%test _ = eq_perm (get_destinations 
(AFD
   ( [ 0; 1 ],
     [ 'a'; 'b' ],
     [ (0, 'a', 1); (0, 'a', 0); (1, 'b', 1) ],
     0,
     [ 1 ] )) 0 'a') [1;0]

let%test _ = eq_perm (get_destinations
(AFD
( [ "q0"; "q1"; "q2" ],
  [ 0; 1 ],
  [ ("q0", 1, "q0"); ("q0", 1, "q1"); ("q0", 1, "q2") ],
  "q0",
  [ "q1"; "q2" ] ))
"q0"
1
) ["q0"; "q1"; "q2"]



(* destination_par_etat_transition : ('a,'b) afnd -> (('b * 'a transition)*'b list) list *)
(* pour l'automate [a], renvoies une liste associative où les clées sont les couples (état, transition) *)
(* et les valeurs, la liste des états ateignables depuis l'état par la transitions *)
(* Tous les couples (état, transition) doivent être présent dans la liste *)
let destinations_par_etat_transition (AFD(etats,alpha,tr,e,f)) =
  (*Flatten en raison du double map donnant un a' list list avec comme unique élément le résultat*)
  List.flatten (List.map (fun lettre -> 
    List.map (fun stat -> 
      ((stat,lettre),get_destinations (AFD(etats,alpha,tr,e,f)) stat lettre)
      ) etats
    ) alpha)



let%test _ =
  eq_perm
    (destinations_par_etat_transition afd1)
    [ ((0, 'a'), [ 1 ]); ((0, 'b'), []); ((1, 'a'), [ 0 ]); ((1, 'b'), [ 1 ]) ]

let%test _ =
  eq_perm
    (destinations_par_etat_transition afd2)
    [
      (("q0", 0), []);
      (("q0", 1), [ "q1" ]);
      (("q1", 0), [ "q1" ]);
      (("q1", 1), [ "q2" ]);
      (("q2", 0), []);
      (("q2", 1), []);
    ]

let%test _ =
  eq_perm
    (destinations_par_etat_transition afd3)
    [
      (("q0", 'a'), [ "q1" ]);
      (("q0", 'b'), [ "q3" ]);
      (("q0", 'c'), []);
      (("q1", 'a'), []);
      (("q1", 'b'), [ "q4" ]);
      (("q1", 'c'), [ "q2" ]);
      (("q2", 'a'), []);
      (("q2", 'b'), [ "q5" ]);
      (("q2", 'c'), [ "q1" ]);
      (("q3", 'a'), [ "q4" ]);
      (("q3", 'b'), [ "q6" ]);
      (("q3", 'c'), []);
      (("q4", 'a'), []);
      (("q4", 'b'), [ "q5" ]);
      (("q4", 'c'), [ "q6" ]);
      (("q5", 'a'), [ "q7" ]);
      (("q5", 'b'), [ "q5" ]);
      (("q5", 'c'), []);
      (("q6", 'a'), [ "q6" ]);
      (("q6", 'b'), []);
      (("q6", 'c'), [ "q4" ]);
      (("q7", 'a'), [ "q6" ]);
      (("q7", 'b'), [ "q4" ]);
      (("q7", 'c'), []);
    ]

let%test _ = eq_perm
(destinations_par_etat_transition (AFD
( [ 0; 1 ],
  [ 'a'; 'b' ],
  [ (0, 'a', 1); (0, 'a', 0); (1, 'b', 1) ],
  0,
  [ 1 ] )))
[((0, 'a'), [1; 0]); ((0, 'b'), []); ((1, 'a'), []); ((1, 'b'), [1])]
||
eq_perm
(destinations_par_etat_transition (AFD
( [ 0; 1 ],
  [ 'a'; 'b' ],
  [ (0, 'a', 1); (0, 'a', 0); (1, 'b', 1) ],
  0,
  [ 1 ] )))
[((0, 'a'), [0; 1]); ((0, 'b'), []); ((1, 'a'), []); ((1, 'b'), [1])]

(* ('a,'b) afnd -> bool *)
(* Indique si l'automate est déterministe *)
let est_deterministe  (AFD(etats,alpha,tr,e,f)) = 
    List.for_all (fun (_,reacheable_states) -> (List.length reacheable_states) <= 1) (destinations_par_etat_transition (AFD(etats,alpha,tr,e,f)) ) 


let%test _ = est_deterministe afd1
let%test _ = est_deterministe afd2
let%test _ = est_deterministe afd3
let%test _ = not (est_deterministe (AFD
( [ 0; 1 ],
  [ 'a'; 'b' ],
  [ (0, 'a', 1); (0, 'a', 0); (1, 'b', 1) ],
  0,
  [ 1 ] )))
let%test _ = not (est_deterministe (AFD
( [ "q0"; "q1"; "q2" ],
  [ 0; 1 ],
  [ ("q0", 1, "q0"); ("q0", 1, "q1"); ("q0", 1, "q2") ],
  "q0",
  [ "q1"; "q2" ] )))


(* appartient_langage_afd ('a,'b) afd -> 'a list -> bool *)
(* Vérifie si le mot [mot] appartient_langage_afd au langage associé à l'automate [a] *)

exception AutomateNonDeterministe

let appartient_langage_afd (AFD(etats,alpha,tr,e,f)) mot =
      let rec aux_appartient_langage_afd start_state mot =
        match mot with
        | [] -> true (* Par construction, le mot vide appartient à tout AFD [On reste sur l'état initial]*)
        | t::q -> 
            let next = get_destinations (AFD(etats,alpha,tr,e,f)) start_state t in 
            (* appartient_mot "mot" avec AFD démarrant à q0 = appartient_mot "ot" avec AFD démarrant à Target(q0-"m")*)
            (* Fin de la récursion quand mot vide *)
              match next with
              | [] -> false (* Le mot est non-vide mais ne présente plus de transitions *)
              | [a] -> aux_appartient_langage_afd a q
              | _ -> raise AutomateNonDeterministe
        in
          if est_deterministe (AFD(etats,alpha,tr,e,f)) then
            aux_appartient_langage_afd e mot
          else
            raise AutomateNonDeterministe

(* A : L(A) = {m ∈ X∗| ˆδ(qI , m) ∈ F } *)
(* Deux cas, soit m est vide soit l'extension (qI,m) est un état final*)

(* let extension (AFD(etats,alpha,tr,e,f)) mot =
  let rec aux_extension acc reste etat_init=
    match reste with
    | [] -> acc
    | t::q ->
       let (_,list) = List.find (fun ((src,symb),_) -> 
        src=etat_init && symb=t
        ) (destinations_par_etat_transition (AFD(etats,alpha,tr,e,f)))
       in
       aux_extension (list@acc) q (
        match list with
        | [] -> e
        | t::_ -> t
       )
      in aux_extension [] mot e


let appartient_langage_afd (AFD(etats,alpha,tr,e,f)) mot =
  if est_deterministe (AFD(etats,alpha,tr,e,f)) then
    if mot = [] then
      true
    else
     List.mem (List.hd (List.rev (extension (AFD(etats,alpha,tr,e,f)) mot))) f
  else
    raise AutomateNonDeterministe *)

      

let%test _ = appartient_langage_afd afd1 [ 'a' ]
let%test _ = appartient_langage_afd afd1 [ 'a'; 'b' ]
let%test _ = appartient_langage_afd afd1 [ 'a'; 'b'; 'b' ]
let%test _ = appartient_langage_afd afd1 [ 'a'; 'b'; 'b'; 'a'; 'a' ]

let%test _ =
  appartient_langage_afd afd1 [ 'a'; 'b'; 'b'; 'a'; 'a'; 'b'; 'b'; 'b' ]

let%test _ = appartient_langage_afd afd1 [ 'a'; 'b'; 'a'; 'a' ]
let%test _ = not (appartient_langage_afd afd1 [])
let%test _ = not (appartient_langage_afd afd1 [ 'b' ])
let%test _ = not (appartient_langage_afd afd1 [ 'c' ])
let%test _ = not (appartient_langage_afd afd1 [ 'a'; 'b'; 'a' ])
let%test _ = not (appartient_langage_afd afd1 [ 'a'; 'b'; 'a'; 'b' ])
let%test _ = appartient_langage_afd afd2 [ 1 ]
let%test _ = appartient_langage_afd afd2 [ 1; 1 ]
let%test _ = appartient_langage_afd afd2 [ 1; 0 ]
let%test _ = appartient_langage_afd afd2 [ 1; 0; 1 ]
let%test _ = appartient_langage_afd afd2 [ 1; 0; 0 ]
let%test _ = appartient_langage_afd afd2 [ 1; 0; 0; 1 ]
let%test _ = appartient_langage_afd afd2 [ 1; 0; 0; 0 ]
let%test _ = appartient_langage_afd afd2 [ 1; 0; 0; 0; 1 ]
let%test _ = appartient_langage_afd afd2 [ 1; 0; 0; 0; 0 ]
let%test _ = not (appartient_langage_afd afd2 [])
let%test _ = not (appartient_langage_afd afd2 [ 0 ])
let%test _ = not (appartient_langage_afd afd2 [ 1; 1; 1 ])
let%test _ = not (appartient_langage_afd afd2 [ 0; 1 ])
let%test _ = not (appartient_langage_afd afd2 [ 1; 0; 1; 1 ])
let%test _ = not (appartient_langage_afd afd2 [ 0; 0; 0 ])
let%test _ = appartient_langage_afd afd3 []
let%test _ = appartient_langage_afd afd3 [ 'a'; 'c' ]
let%test _ = appartient_langage_afd afd3 [ 'a'; 'c'; 'c'; 'c' ]
let%test _ = appartient_langage_afd afd3 [ 'a'; 'c'; 'b'; 'a' ]
let%test _ = appartient_langage_afd afd3 [ 'a'; 'c'; 'b'; 'a'; 'a' ]
let%test _ = appartient_langage_afd afd3 [ 'b'; 'b' ]
let%test _ = appartient_langage_afd afd3 [ 'b'; 'b'; 'a' ]
let%test _ = appartient_langage_afd afd3 [ 'b'; 'b'; 'a'; 'a' ]
let%test _ = appartient_langage_afd afd3 [ 'b'; 'b'; 'c'; 'c' ]
let%test _ = appartient_langage_afd afd3 [ 'b'; 'b'; 'c'; 'c'; 'a' ]
let%test _ = appartient_langage_afd afd3 [ 'b'; 'a'; 'c' ]
let%test _ = appartient_langage_afd afd3 [ 'a'; 'b'; 'c' ]
let%test _ = appartient_langage_afd afd3 [ 'a'; 'b'; 'b'; 'a' ]
let%test _ = not (appartient_langage_afd afd3 [ 'a' ])
let%test _ = not (appartient_langage_afd afd3 [ 'b' ])
let%test _ = not (appartient_langage_afd afd3 [ 'c' ])
let%test _ = not (appartient_langage_afd afd3 [ 'a'; 'b' ])
let%test _ = not (appartient_langage_afd afd3 [ 'b'; 'a' ])
let%test _ = not (appartient_langage_afd afd3 [ 'b'; 'a'; 'b' ])
let%test _ = not (appartient_langage_afd afd3 [ 'a'; 'b'; 'b' ])
let%test _ = not (appartient_langage_afd afd3 [ 'a'; 'b'; 'a' ])
let%test _ = not (appartient_langage_afd afd3 [ 'c'; 'a' ])
let%test _ = not (appartient_langage_afd afd3 [ 'c'; 'b' ])

let%test _ = 
try
  let _ = appartient_langage_afd (AFD
  ( [ 0; 1 ],
    [ 'a'; 'b' ],
    [ (0, 'a', 1); (0, 'a', 0); (1, 'b', 1) ],
    0,
    [ 1 ] )) ['a'] in false
with AutomateNonDeterministe -> true

let%test _ = 
try
  let _ = appartient_langage_afd (AFD
  ( [ 0; 1 ],
    [ 'a'; 'b' ],
    [ (0, 'a', 1); (1, 'b', 0); (1, 'b', 1) ],
    0,
    [ 1 ] )) ['a'] in false
with AutomateNonDeterministe -> true

let%test _ = 
try
  let _ = appartient_langage_afd (AFD
( [ "q0"; "q1"; "q2" ],
  [ 0; 1 ],
  [ ("q0", 1, "q0"); ("q0", 1, "q1"); ("q0", 1, "q2") ],
  "q0",
  [ "q1"; "q2" ] )) [1] in false
with AutomateNonDeterministe -> true 
