(******* TRIS ******)

(*  Tri par insertion **)

(*CONTRAT
Fonction qui ajoute un élément dans une liste triée, selon un ordre donné
Type : ('a->'a->bool)->'a->'a list -> 'a list
Paramètre : ordre  ('a->'a->bool), un ordre sur les éléments de la liste
Paramètre : elt, l'élement à ajouter
Paramètre : l, la liste triée dans laquelle ajouter elt
Résultat : une liste triée avec les éléments de l, plus elt
*)

let rec fold_right f l e =
  match l with
  | [] -> e
  | t :: q -> f t ( fold_right f q e)

let rec insert ordre elt l = match l with
  | [] -> [elt]
  | tete::queue -> if ordre elt tete then
        elt::l
  else
        tete::(insert ordre elt queue);;

(* TESTS *)
let%test _ = insert (fun x y -> x<y) 3 []=[3]
let%test _ = insert (fun x y -> x<y) 3 [2;4;5]=[2;3;4;5]
let%test _ = insert (fun x y -> x > y) 6 [3;2;1]=[6;3;2;1]



(*CONTRAT
Fonction qui trie une liste, selon un ordre donné
Type : ('a->'a->bool)->'a list -> 'a list
Paramètre : ordre  ('a->'a->bool), un ordre sur les éléments de la liste
Paramètre : l, la liste à trier
Résultat : une liste triée avec les éléments de l
*)

(*let rec tri_insertion ordre l = match l with
  | [] -> []
  | tete::queue -> insert ordre tete (tri_insertion ordre queue);;*)

let tri_insertion ordre l = fold_right (insert ordre) l [];;

(* TESTS *)
let%test _ = tri_insertion (fun x y -> x<y) [] =[]
let%test _ = tri_insertion (fun x y -> x<y) [4;2;4;3;1] =[1;2;3;4;4]
let%test _ = tri_insertion (fun x y -> x > y) [4;7;2;4;1;2;2;7]=[7;7;4;4;2;2;2;1]


(*  Tri fusion **)

(* CONTRAT
Fonction qui décompose une liste en deux listes de tailles égales à plus ou moins un élément
Paramètre : l, la liste à couper en deux
Retour : deux listes
*)

let rec scinde l =  match l with 
    | [] -> [],[]
    | [_] -> l,[]
    | tete::snd::queue -> 
        let (l1,l2) = (scinde queue) in (tete::l1,snd::l2);;
      (*Redéfinition locale de deux listes ayant chacune tete et snd en tête
         puis découpage de la queue restante, avec l1,l2 résultats récursifs de scinde sur queue*)


(* TESTS *)
(* Peuvent être modifiés selon l'algorithme choisi *)
let%test _ = scinde [1;2;3;4] = ([1;3],[2;4])
let%test _ = scinde [1;2;3] = ([1;3],[2])
let%test _ = scinde [1] = ([1],[])
let%test _ = scinde [] = ([],[])


(* Fusionne deux listes triées pour en faire une seule triée
Paramètre : ordre  ('a->'a->bool), un ordre sur les éléments de la liste
Paramètre : l1 et l2, les deux listes triées
Résultat : une liste triée avec les éléments de l1 et l2
*)

let rec fusionne ordre l1 l2 = match l1,l2 with
      | [],_ -> l2
      | _,[] -> l1
      | t1::q1,t2::q2 -> 
          if ordre t1 t2 then
            t1::(fusionne ordre q1 l2)
          else
            t2::(fusionne ordre l1 q2);;


(*TESTS*)
let%test _ = fusionne (fun x y -> x<y) [1;2;4;5;6] [3;4] = [1;2;3;4;4;5;6]
let%test _ = fusionne (fun x y -> x<y) [1;2;4] [3;4] = [1;2;3;4;4]
let%test _ = fusionne (fun x y -> x<y) [1;2;4] [3;4;8;9;10] = [1;2;3;4;4;8;9;10]
let%test _ = fusionne (fun x y -> x<y) [] [] = []
let%test _ = fusionne (fun x y -> x<y) [1] [] = [1]
let%test _ = fusionne (fun x y -> x<y) [] [1] = [1]
let%test _ = fusionne (fun x y -> x<y) [1] [2] = [1;2]
let%test _ = fusionne (fun x y -> x>y) [1] [2] = [2;1]


(* CONTRAT
Fonction qui trie une liste, selon un ordre donné
Type : ('a->'a->bool)->'a list -> 'a list
Paramètre : ordre  ('a->'a->bool), un ordre sur les éléments de la liste
Paramètre : l, la liste à trier
Résultat : une liste triée avec les éléments de l
*)

let rec tri_fusion ordre l =
  let tri_fusion_ordre = tri_fusion ordre in
  let fusionne_ordre = fusionne ordre in
  let scinde_l = scinde l in
  match l with 
  | [] -> []
  | [_] -> l
  | _ -> let (l1,l2) = scinde_l in fusionne_ordre (tri_fusion_ordre l1) (tri_fusion_ordre l2);;


(* TESTS *)
let%test _ = tri_fusion (fun x y -> x<y) [] =[]
let%test _ = tri_fusion (fun x y -> x<y) [4;2;4;3;1] =[1;2;3;4;4]
let%test _ = tri_fusion (fun x y -> x > y) [4;7;2;4;1;2;2;7]=[7;7;4;4;2;2;2;1]


(*  Parsing du fichier *)
open Lexing

(* Affiche un quadruplet composé 
- du sexe des personnes ayant reçu ce prénom : 1 pour les hommes, 2 pour les femmes
- du prénom
- de l'année
- du nombre de fois où ce prénom a été donné cette année là
*)
let print_stat (sexe,nom,annee,nb) =
  Printf.eprintf "%s,%s,%d,%d%!\n" (if (sexe=1) then "M" else "F") nom annee nb

(* Analyse le fichier nat2016.txt (stratistique des prénoms entre 1900 et 2016) 
 et construit une liste de quadruplet (sexe,prénom,année,nombre d'affectation)
*)
let listStat = 
  let input = open_in "./nat2016.txt" in 
  let filebuf = Lexing.from_channel input in
  Parser.main Lexer.token filebuf
  

(* Analyse le fichier nathomme2016.txt (stratistique des prénoms d'homme commençant par un A ou un B entre 1900 et 2016) 
 et construit une liste de quadruplets (sexe,prénom,année,nombre d'affectations)
*)
let listStatHomme = 
  let input = open_in "./nathomme2016.txt" in
  let filebuf = Lexing.from_channel input in
  Parser.main Lexer.token filebuf
  

(*  Les contrats et les tests des fonctions suivantes sont à écrire *)


let tri_insertion_list_stat_homme = tri_insertion (fun (_,_,_,x) (_,_,_,y) -> x<y) listStatHomme;;

let t = List.sort (fun (_,_,_,x) (_,_,_,y) -> x-y) listStatHomme;;

let scinde_terminal liste =
  let rec aux l acc i =
  match l with
  | [] -> [],[]
  | [_] -> liste,[]
  | t::q -> if i = 0 then (acc,l) else aux q (t::acc) (i-1)
  in aux liste [] (List.length liste / 2);;

let fusionne_terminal ordre l1 l2 =
  let rec aux l1 l2 acc =
    match l1,l2 with
     | [],_ -> l2
     | _,[] -> l1
     | t1::q1,t2::q2 -> if not(ordre t1 t2) then 
        aux  l1 (t1::acc) q2
    else
        aux q1 (t2::acc) l2
    in aux l1 l2 []
    ;;

    (*a' -> a' list list -> a' list list*)
 

      let rec map f liste = match liste with
| [] -> []
| tete::queue -> (f tete)::(map f queue);;
      let rec insertion e l =
        match l with
        | [] -> [[e]]
        | t::q -> [[e;t]]@(map (fun qmap -> [t]@qmap) (insertion e q));;

        let rec insertion_z e l =
          match l with
          | [] -> [[e]]
          | t::q -> (e::l)::(List.map (fun qmap -> t::qmap) (insertion_z e q));;


          let rec permutations ensemble = 
            match ensemble with
            | [] -> [[]]
            | t::q -> List.flatten ( List.map (fun qq -> insertion_z t qq) (permutations q) );;

            let permutations ensemble =
              List.fold_right ensemble [[]] ( (fun _ t q -> 
              List.flatten( List.map (fun qq -> insertion_z t qq) q)));;