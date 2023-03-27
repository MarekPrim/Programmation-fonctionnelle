### Exercice 1

```ocaml
type 'a arbre = Node of boolean * a' branche list
and 'a branche = a' * a' arbre
(*Type mutuellement récursif*)

type 'a arbre = Node of bool * (a' * a' arbre) list
```

### Exercice 2

```ocaml
a' list -> a' arbre -> bool

let appartient els arbre = 
	tree_fold (fun _ liste_appartient_fils -> List.fold_right (fun t q -> t=els) liste_appartient_fils false ) arbre


	(* a' -> a' arbre -> a' arbre option *)
	(* a' * ('a * 'b) list -> b' option *)
	let rec recherche el sous_arbre =
	match sous_arbre with
	| [] -> None
	| (t,at)::q -> 
		if el > t
			recherche e q
		else if el = t
			Some at
		else

let rec appartient m (Node (b,edges)) =
match m with
| [] -> b
| t::q ->
	match with recherche t edges with
		| None -> false
		| Some sa -> appartient q sa

appartient [l(false);e(false);s(true)] a
appartient [e(false);s(true)] al
appartient [s(true)] ale
appartient [(true)] ales
```

### Exercice 3

```ocaml
(*
Deux cas pour ajout :

 - Ajouter un mot existant (switch un boolean)

 - Ajouter un mot partiellement inexistant (ajoute une branche)
*)
(* 'a -> (a'*'b) -> (a'*b') list -> ('a*b') list  *)
(*MAJ*)

let rec mise_a_jour character new_branch all_branches =
match all_branches with
 | [] -> [(character,new_branch)]
 | (t_c,t_a)::qu_branch ->
		 if t_c > character then
			 (character,new_branch)::all_branches
	    else if t_c = character then
		    (character,new_branch)::qu_brachn
		else
			(t_c,t_a)::(mise_a_jour character new_branch qu_branch)

(* 'a list -> 'a arbre -> 'a arbre *)
(*AJOUT*)

let rec ajout mot (Noeud (bol,edges)) =
match mot with
| [] -> Noeud(true,edges)
| c::qc ->
		match recherche c edges with
		| None ->
			let new_arbre = 
				ajout qc (Node (false, []))
			in Noeud (b, maj c new_arbre edges)
		| Some ac ->
			let na = 
				ajout sq ac
			in Node (b, maj c na edges)







(* ##CORRECTION## *)
(* 'a -> 'b -> ('a*'b) list -> ('a*'b) list *)

mise_ajour 'y' 3 [('y',4);('z',5)] = [('y',3);('z',5)] 

let rec mise_a_jour c v lassoc =
match lassoc with
| [] -> [(c,v)]
| (key,value_key)::q -> 
		if c > key then
			(k,vk)::(mise_a_jour c v q)
		else if c=key then
			(key,v)::q
		else
			(c,v)::(key,value_key)::q



```