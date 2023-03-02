### Exercice 1
1)

```ocaml

Liste à un élément = [_]

(*'a list -> 'a *)
(* Précondition : Taille de la liste >= 2 *)

(* On peut se contenter d'un seul match with *)
(* Filtrage exhaustif *)
let deuxieme liste = match liste with
| [] -> failwith "Liste vide"
| t::q -> 
	begin
		match q with
		| [] -> failwith "Liste à un élément"
		| t::q -> t
	end;;

(* Filtrage exhaustif, mieux que la version 1 *)
let deuxieme l = match l with
| [] -> fw
| _::[] -> fw
| _::x::_ -> x;;

(*Not good *)
let deuxieme l = 
	let _::q = l
	in let snd::_ = q
	in snd;;

(*Not good *)
let deuxieme l = let _::snd::_ in snd;;

(*Filtrage simple, vaut mieux le faire au niveau des arguments MAIS filtrage non exhaustif*)
(*Not good*)
let deuxieme (_::snd::_) = snd;;

(*Meilleure version de la v2*)
(* Correction *)
let deuxieme l = match l with
| _::snd::_ -> snd
| _ -> failwith "Liste non valide"

```

2)

```ocaml

let rec n_a_zero n = 
if n > 0 then
	n::n_a_zero (n-1)
else 
	[]
;;
let rec zero_a_n n = list.rev(n_a_zero n);; (* 2^n *)

let rec zero_a_n n = 
	let rec aux i =
		if n=i then [n]
		else i::(aux n (i+1) )
	in aux 0;;

let rec zero_a_n n =
	let rec aux x = 
		if x > 0 then
			(n-x)::(aux (x-1))
		else
			[n]
	in aux n;;

```


```ocaml


let rec indice_l liste e = match liste with
| [] -> []
| t::q -> 
	if e=t then
		0::List.map(fun x -> x+1) (indice_l q e)
	else
		List.map(fun x -> x+1) (indice_l q e);;

let indice_l liste e = fold_right ( fun _ x indi_queue -> if (x=e) then 0::map(fun x -> x+1) indi_queue else map(fun x -> x+1) indi_queue) liste [];;

```

### Exercice 3

1)

```ocaml
let map f l = fold_right ( fun _ t q -> (f t)::q) [] l;;
```

2)

```ocaml
(* a' list list -> a' list *)

let rec flatten l_l = match l_l with
| [] -> []
| t::q -> t@(flatten q);;


let flatten l_l = fold_right ( fun t q -> t@q) l_l [];;

let flatten l = fold_right (@) l []; (* (@) eq. fun t q -> t@q *)

```

```ocaml

let rec fsts l_c = match l_c with
| [] -> []
| (x,y)::q -> x::(fsts q);;

let fsts = map fst;;

let fsts l_c = map (fun (x,y) -> x) l_c;;

let fsts l_c = fold_right ( fun (x,y) q -> x@q) l_c [];;
```

```ocaml
	let split list = fold_right (fun (x,y) (q1,q2) -> (x::q1,y::q2)) list ([],[]);; 
```

```ocaml

let removeDoublons l = match l with
| [] -> []
| t::q -> 
	if List.mem t q then
		removeDoublons q
	else
		tete::(removeDoublons q)
	;;


let deleteDuplicate l =
	let aux x acc = if List.mem x acc then acc else x::acc
	in fold_right l [] aux;;

```