exception ZeroException

(*  decompose : int -> int list *)
(*  Décompose un entier i en une liste de chiffres codés par des entiers entre 0 et 9 *)
(*  Pré-condition : i > 0 *)
let rec decompose number =
  let rec aux i =
  if i < 10  then
    i::[]
  else
    (i mod 10)::(aux (i/10))
  in List.rev (aux number)

let%test _ = decompose 248 = [2;4;8]
let%test _ = decompose 1 = [1]
let%test _ = decompose 248 != [8;4;2]



(*  recompose : int list -> int *)
(*  Recompose un entier à partir d'une liste de chiffres codés par des entiers entre 0 et 9, en concaténant / juxtaposant les entiers *)
(*  Pré-condition : pour tout élément e de la liste 0 <= e <= 9 *)
(*  Exception : ZeroException si l est vide ou s'il y a un 0 en tête de l *)


let rec recompose liste =
  let rec aux l acc =
    List.fold_left (fun prems dernier -> prems*10+dernier) acc l
  in 
  match liste with
  | [] -> raise ZeroException 
  | 0::_ -> raise ZeroException 
  | list -> aux list 0


let%test _ = recompose [2;4;8] = 248
let%test _ = recompose [1] = 1

let%test _ = 248 = recompose ( decompose 248)