
```ocaml
fold_left f c l -> f prend un accumulateur dans lequel on a déjà traité n-1 premiers éléments et le dernier élément
Le fold_left permet une approche plus impérative, avec un accumulateur

fold_right f l e -> Suit la récursivité naturelle des listes ( f prend la tête et la queue traitée)
```

```ocaml

let reverse liste = fold_left (fun acc d -> d::acc) [] liste
let reverse liste = fold_right (fun t revq -> revq@[t]) liste []
```

```ocaml
let somme liste = fold_left (fun acc d -> acc+d) 0 liste
let somme liste = fold_left (+) 0 l

let somme liste = fold_right (fun t somq -> t+somq) liste 0
let somme liste = fold_right (+) l 0
```

## Padovan efficace

P(n+3) = P(n+1) + P(n)
P(2) = 1
P(1) = P(0) = 0

```ocaml
let rec padovan n =
match n with
| 0 -> 0
| 1 -> 0
| 2 -> 1
| _ -> padovan(n-2)+padovan(n-3)
```

```ocaml

let padovan n =
	let rec aux p pp pp+1 pp+2 =
		if p = n then
			pp
		else
			aux (p+1) (pp+1) (pp+2) ((pp+1)+pp)
	in aux 0 0 0 1
		 
```