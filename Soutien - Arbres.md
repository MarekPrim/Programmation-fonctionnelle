```ocaml
(* 'a arbre -> 'a list list *)
let rec parcours_arbre (Noeud (b,lbranche)) =

	(*List.flatten (List.map parcours_branche lbranche)*)
	let mots = List.flatten (List.map (fun branche -> parcours_branche branche)  lbranche) in
	if b then []::mots else mots 

(* (a'*a' arbre) -> a' list list *)
and parcours_branche (c,a) =
	List.map ( fun mot_sous_arbre -> c::mot_sous_arbre) (parcours_arbre a)

```

```
and -> défition simulatanée pour des fonctions mutuellement récursives
```
