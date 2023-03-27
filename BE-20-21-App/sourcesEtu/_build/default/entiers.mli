(*  decompose : int -> int list *)
(*  Décompose un entier i en une liste de chiffres codés par des entiers entre 0 et 9 *)
(*  Pré-condition : i > 0 *)
val decompose : int -> int list

(*  recompose : int list -> int *)
(*  Recompose un entier à partir d'une liste de chiffres codés par des entiers entre 0 et 9, en concaténant / juxtaposant les entiers *)
(*  Pré-condition : pour tout élément e de la liste 0 <= e <= 9 *)
(*  Exception : ZeroException si l est vide ou s'il y a un 0 en tête de l *)
val recompose: int list -> int