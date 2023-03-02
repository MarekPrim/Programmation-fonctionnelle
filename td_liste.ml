let head liste = match liste with
| [] -> failwith "liste vide"
| tete::queue -> tete;;

let queue liste = match liste with
| [] -> failwith "liste vide"
| tete::queue -> queue;;

let rec size liste = match liste with
| [] -> 0
| tete::queue -> 1 + size queue;;

let rec append liste1 liste2 = match liste2 with
| [] -> liste1
| tete::queue -> tete::(append queue liste1);;

let rec fold_left f a liste = match liste with
| [] -> a
| tete::queue -> fold_left f (f a tete) queue;;

let rec fold_right f liste a = match liste with
| [] -> a
| tete::queue -> f tete (fold_right f queue a);;

let rev_left liste = fold_left (fun a x -> x::a) [] liste;;

let rev_right liste = fold_right (fun x a -> a@[x]) liste [];;

let deuxieme liste = match liste with
| [] -> failwith "liste vide"
| tete::queue -> match queue with
| [] -> failwith "liste trop courte"
| tete::queue -> tete;;

(* zero_a_n n = [0;1;2;...;n-1;n] *)

(* Build a list of position of element i in list liste *)
let indice_l liste i = 
let rec aux liste i acc = match liste with
| [] -> acc
| tete::queue -> if tete = i then aux queue i (acc@[i]) else aux queue i acc
in aux liste i [];;

(* Build a list of position of element i in list liste with a fold*)


(* Build a list of position of element i in list liste with a fold*)

let rec flatten l_l = match l_l with
| [] -> []
| t::q -> t@(flatten q);;

let rec fold_right l e f = (* e = cas de base *)
match l with 
| [] -> e
| t::q -> f t (fold_right q e f);;

let flatten l_l = fold_right l_l [] ( fun t q -> t@q);;

let fsts l_c = fold_right l_c [] ( fun (x,y) q -> x@q);;

let split l_c = fold_right l_c ([],[]) ( fun (x,y) (q1,q2) -> (x::q1,y::q2));;

(*Remove duplicate in a list*)
let deleteDuplicate l = 
let rec aux l acc = match l with
| [] -> acc
| tete::queue -> if List.mem tete acc then aux queue acc else aux queue (tete::acc)
in aux l [];;

(*Remove duplicate in a list with a fold*)
let deleteDuplicate l = 
let aux x acc = if List.mem x acc then acc else x::acc
in fold_right l [] aux;;

(*Remove duplicate in a list with a fold*)
let deleteDuplicate l = 
let aux x acc = if List.mem x acc then acc else x::acc
in fold_right l [] aux;;

let rec deleteDuplicate l = match l with
| [] -> []
| tete::queue -> if List.mem tete queue then deleteDuplicate queue else tete::(deleteDuplicate queue);;

let rec deleteDuplicate l = match l with
| [] -> []
| tete::queue -> if List.mem tete queue then deleteDuplicate queue else tete::(deleteDuplicate queue);;

let rec deleteDuplicate l = match l with
| [] -> []
| tete::queue -> if List.mem tete queue then deleteDuplicate queue else tete::(deleteDuplicate queue);;

let rec deleteDuplicate l = match l with
| [] -> []
| tete::queue -> if List.mem tete queue then deleteDuplicate queue else tete::(deleteDuplicate queue);;

let rec deleteDuplicate l = match l with
| [] -> []
| tete::queue -> if List.mem tete queue then deleteDuplicate queue else tete::(deleteDuplicate queue);;

let rec deleteDuplicate l = match l with
| [] -> []
| tete::queue -> if List.mem tete queue then deleteDuplicate queue else tete::(deleteDuplicate queue);;

let rec deleteDuplicate l = match l with
| [] -> []
| tete::queue -> if List.mem tete queue then deleteDuplicate queue else tete::(deleteDuplicate queue);;

let rec deleteDuplicate l = match l with
| [] -> []
| tete::queue -> if List.mem tete queue then deleteDuplicate queue else tete::(deleteDuplicate queue);;

let rec deleteDuplicate l = match l with
| [] -> []
| tete::queue -> if List.mem tete queue then deleteDuplicate queue else tete::(deleteDuplicate queue);;

(* delete duplicates with fold_right*)
let deleteDuplicate l = nan;;