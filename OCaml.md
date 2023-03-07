
Pas d'effet de bord, tout est une expression.
Fonction au sens mathématique : Domaine -> Domaine
Pas de séquence : Pas de boucle, de if; en tout cas, pas au sens impératif. Pas de for, ni de séquence.
Pas d'environement global : Appel avec les mêmes arguments, toujours le même résultat
Indépendant de l'ordre d'évaluation et du contexte d'éxecution
Typage fort

Controle de type : Le compilateur regarde que les opérations ne violent pas les contraintes de types (Java, Ada, ...)

Inférence de type : Le compilateur essaie de deviner les types (OCaml fait majoritairement de l'inférence)
Pas de variables mais des identificateurs. Forcément déclarés et initialisés.

Notation fonction :

divise x y : int -> int -> bool

Pour OCaml, la fonction divise prend un entier et renvoie une fonction prenant un entier et renvoyant un booléen

Un filtre définit avec :

```ocaml
# let f x a b c = match x with 
|F1 -> a 
|F2 -> b 
|_ -> c
```

aura comme type :
```
val f : int * int -> 'a -> 'a -> 'a -> 'a = <fun>
```
Ainsi, les retours d'un filtre doivent être du même type.
On peut déterminer le type de l'expression selon les valeurs des branches F1 et F2.

```ocaml
let isequal x n = match x with

| n -> true

| _ -> false;;
```
Renvoie toujours true car le n dans le premier cas est un masquage de la variable n, avec n' = x, d'où le passage obligatoire dans le premier cas.
Le filtrage est une **affectation**, et non une **conditionelle**.

## Liste et itérateurs de liste

Liste et entier sont des types récursifs -> Entier soit 0 soit suivant; Entier soit 0 soit pair/impair

Liste -> [] ou a::l
```ocaml
:: : 'a -> 'a list -> 'a list

a::b::c::[] eq. [a; b; c]
```

Une liste est considérée comme étant [tête, ...queue]

Dans un cas d'erreur, on ne renvoie pas de valeur par défaut (Cf. Ariane 5)

```ocaml
let hd liste = match liste with
| [] -> failwith "Vide"
| tete::_ -> tete;;

let tl liste = match liste with
| [] -> failwith "Vide"
| _::queue -> queue;;

let rec size liste = match liste with
| [] -> 0
| _::queue -> 1 + size queue;;

let rec append l1 l2 = match l1 with
| [] -> l2
| tete::queue -> tete::(append queue liste2);;
(* Récursion terminal, donc on empile les appels à append donc pas d'inversion de l'ordre de la liste *)
```

```ocaml

List.map : (a' -> b') -> 'a list -> 'b list

let rec map f liste = match liste with
| [] -> []
| tete::queue -> (f tete)::(map f queue);;

let string_of_int_list = map string_of_int 
(* Application partielle, renvoie une fonction qui prend une liste et renvoie une liste *)
```

```ocaml
let rec fold_right l e f = (* e = cas de base *)
match l with 
| [] -> e
| t::q -> f t (fold_right q e f);;

let taille l = 
fold_right l 0 (fun _ tete size_queue -> size_queue+1);;

let map f l = 
	fold_right (fun _ t map_queue -> (f t)::map_queue);;

let append l1 l2 =
	fold_right ( fun _ t1 q1l2 -> t1::q1l2) l1 l2
```

fold_right se généralise sur toute structure de données

```ocaml
fold_right : ('a = tete de la liste -> 'b = résultat sur la queue de la liste -> 'b = résultat sur la liste ) -> 'a list -> 'b -> 'b
```

```ocaml
fold_left : ( 'b -> 'a -> 'b ) -> 'b -> a' list -> 'b
```

```ocaml

let rev_right list = fold_right (fun t revq -> revq@[t] ) list [];; (*O(n^2)*)

let rev_left list = fold_left (fun rev_firsts last -> last::rev_firsts) [] list; (* O(n) *)
```

## Définition de types
Un constructeur commence toujours par une majuscule.

Type ``option`` est une structure de donnée qui permet de spécifier un choix entre une situation normale et une situation exceptionnelle.

### Type récursif

Exemple : Personne avec enfant, parents
Réprésentation sous forme d'arbre.
Les types récursifs paramétrés ne sont mêmes pas nécessairement homogènes.
**Powerlist**

![[Pasted image 20230306102939.png]]

### Arbre binaire

![[Pasted image 20230306103436.png]]

``` ocaml
let rec tree_map f arb =
match arb with
| Empty -> Empty
| Node (n,g,d) -> Node (f n, tree_map f g, tree_map f d)
```

```ocaml
let rec tree_fold f e arb =
match arb with
| Empty -> e
| Node (n,g,d) -> f n (tree_fold f e g) (tree_fold f e d)
```

Le fold prend une fonction avec la racine, le fils droit déjà traité, le fils gauche déja traité; et renvoie le résultat.

```ocaml
let rec cardinal arb =
tree_fold (fun _ cd cg -> 1+cd+cg) 0 arb
```

Le fold se généralise sur tous les types OCaml. Il "remplace" les constructeurs du type par des appels de fonctions de même arité.
(Pour les list, Nil=Cas de base et Cons).

```ocaml
let rec pronfondeur a = match a with
| Vide -> 0
| Noeud(g,r,d) -> 1 + pfndeur g

let noeud g r d = 
 if pfd g < pfd d then Noeud (d,r,g) else Noeud(g,r,d)

let rec union arb1 arb2 = 
	match a1,a2 w/
	| Vide,_ -> a2
	| _,Vide -> a1
	| Noeud (g1,r1,d1), Noeud (g2,r2,d2) ->
		if (r1 > r2) then noeud g2 r2 (union a1 d2)
		else noeud g1 r1 (union a2 d1)
```

```ocaml
let insertion el arb =
 union (Noeud (Vide,el,Vide)) arb
```

#### Parcours d'arbres binaires

![[Pasted image 20230306105938.png]]

![[Pasted image 20230306110046.png]]


### Arbre n-aire

```ocaml

type 'a arbre_naire = Noeud of 'a * 'a arbre_naire list

let cons el list_arbre_naire =
Noeud (el, list_arbre_naire )

let racine (Noeud (r,_)) = r

let fils (Noeud (_,fils)) = fils

let rec map_arbre_naire f (Noeud (r, list_arbre_naire)) =
Noeud (f r, List.map (map_arbre_naire f) list_arbre_naire)

let rec fold_arbre_naire f e (Noeud (r,list_arbre_naire)) =
f r (List.map (fold_arbre_naire f ) list_arbre_naire)

let rec cardinal arb =
fold_arbre_naire (fun _ lcf -> 1 + List.fold_right (+) lcf 0) 0 arb
```


### Exercices
Couple (profondeur min, profondeur max)

Sur arbre binaire :

```ocaml
let profondeurs arbre =
tree_fold (fun _ pfmin_g pfmax_g pfmin_d pfmax_d ->          (1 + min pfmin_g pfmin_d),(1+ max pfmax_g pfmax_d)) (0,0) arbre
```

Sur arbre n-aire :

```
Même chose mais on récupère une liste des max et des min.
On doit faire le max des maxs et le min des mins.
```