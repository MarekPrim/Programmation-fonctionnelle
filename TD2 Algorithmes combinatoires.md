
### Exercice 1

<u>Parties</u>

0 -> 1
n+1 -> 2^n (parties de l'ensemble de taille n sans l'élément +1) + 2^n (parties de l'ensemble de taille n avec l'élément +1)

### Exercice 2

```ocaml
a' -> a' list list -> a' list list

ajout 3 [[],[1;2]] = à permutation près [[],[3],[1;2],[3;1;2]]

let rec ajout e ensemble = match ensemble with
 | [] -> []
 | t::q -> t::(e::t)::(ajout e q);

let ajout e ensemble =
	fold_right ensemble [] (fun _ t q -> t::(e::t)::q);;

let ajout e ensemble =
	ensemble@(map (fun tmap -> tmap::e)) ensemble);;

let ajout e ensemble =
	flatten (map (fun ec -> [ec;e::ec]) l);;

```

```ocaml
parties [] = [[]]

parties [1;2;3] = [[],[1],[2],[3],[1;2],[1;3],[2;3],[1;2;3]]

parties [2;3] = [[],[2],[3],[2;3]]

parties [1;2;3] = ajout 1 parties [2;3]
parties [2;3] = ajout 2 parties [3]
parties [3] = ajout 3 parties []

let parties ensemble = 
	fold_right ensemble [[]] ajout;;
```

### Exercice 3

```
a' list list -> int
permutations [[]] = 1
permutations [[],[1]] = 2

permutations (n+1) = (n+1 == element à toutes les positions possibles == INSERTION)*permutations(n)
```

```ocaml
a' -> a' list -> a' list list

insertion 4 [1;2;3] = [[4;1;2;3],[1;4;2;3],[1;2;4;3],[1;2;3;4]]
insertion 4 [2;3] = [[4;2;3],[2;4;3],[2;3;4]]
insertion 4 [1] = [[4;1],[1;4]]
insertion 4 [] = [[4]]

Pas de fold car on utilise la tête, l'appel récursif ET la liste de départ

let rec insertion e l =
	map (fun ec -> [e;ec::l]) l;;

let rec insertion e l =
	match l with
	| [] -> [[e]]
	| t::q -> (e::l)::(map (fun qmap -> t::qmap) (insertion e q));;

let rec insertion e l =
match l with
| [] -> [[e]]
| t::q -> [[e;t]]@(map (fun qmap -> [t]@qmap) (insertion e q));;
On pourrait pensez que cet fonction est correcte mais elle ne donne pas un résultat correct à toute position

```
![[2023-03-02-112012.jpg]]

```ocaml

permutations [1;2;3] = [[1;2;3],[2;1;3],[2;3;1],[1;3;2],[3;1;2],[3;2;1]]

permutations [2;3] = [[2;3],[3;2]]

permuations [1;2;3] = insertion 3 (permutation [2;3])
permutations [2;3] = insertion 3 (permutation [2])
permutations [2] = insertion 2 (permutations [])

let rec permutations ensemble =
match ensemble with
| [] -> []
| t::q -> List.flatten ( List.map (fun qq -> insertion t qq) (permutations q) );;


let permutations ensemble =
	List.fold_right ensemble [[]] (fun t permq -> List.flatten ( List.map ( insertion t) permq));;

Insere un élément (insertion) à toute les permutations possibles (map) de la queue (permq) (flatten pour récupérer résultat de map)

```