let rec combinaison l k = match k,l with
  | (0,_) -> [[]]
  | (_,[]) -> []
  | (n,t::q) ->
      List.map (fun e -> t::e) (combinaison q (n-1)) @ (combinaison q n)


(* TESTS *)
(*Cas particulier : Combinaisons à 0 éléments de nil*)
let%test _ = combinaison [] 0 = [[]]
(*Cas particulier : Combinaisons à 1 éléments de nil*)
let%test _ = combinaison [] 1 = []
(*Cas de base : Combinaison à 1 élément d'un singleton*)
let%test _ = combinaison [1] 1 = [[1]]
(*Cas particulier : Combinaison à 0 éléments d'un singleton*)
let%test _ = combinaison [1] 0 = [[]]
(*Cas de base*)
let%test _ = combinaison [1;2] 1 = [[1];[2]]
let%test _ = combinaison [1;2] 2 = [[1;2]]
let%test _ = combinaison [1;2;3] 2 = [[1;2];[1;3];[2;3]]
let%test _ = combinaison [1;2;3] 3 = [[1;2;3]]
let%test _ = combinaison [1;2;3;4] 2 = [[1;2];[1;3];[1;4];[2;3];[2;4];[3;4]]
let%test _ = combinaison [1;2;3;4] 3 = [[1;2;3];[1;2;4];[1;3;4];[2;3;4]]
let%test _ = combinaison [1;2;3;4] 1 = [[1];[2];[3];[4]]
(*Cas particulier : Combinaison à 0 éléments d'une liste quelconque*)
let%test _ = combinaison [1;2;3;4] 0 = [[]]
(*Cas particulier : Combinaison de k éléments d'une liste de taille s, k>s*)
let%test _ = combinaison [1;2;3;4] 5 = []