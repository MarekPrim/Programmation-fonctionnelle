(******************************************************************************)
(*                                                                            *)
(*      fonction de décomposition pour les chaînes de caractères              *)
(*                                                                            *)
(*   signature : decompose : string -> char list = <fun>               *)
(*                                                                            *)
(*   paramètre(s) : une chaîne de caractères                                  *)
(*   résultat     : la liste des caractères composant la chaîne paramètre     *)
(*                                                                            *)
(******************************************************************************)
let decompose s =
  let rec decompose i accu =
    if i < 0 then accu else decompose (i - 1) (s.[i] :: accu)
  in
  decompose (String.length s - 1) []

let%test _ = decompose "" = []
let%test _ = decompose "a" = [ 'a' ]
let%test _ = decompose "aa" = [ 'a'; 'a' ]
let%test _ = decompose "ab" = [ 'a'; 'b' ]
let%test _ = decompose "abcdef" = [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f' ]

(******************************************************************************)
(*                                                                            *)
(*      fonction de recomposition pour les chaînes de caractères              *)
(*                                                                            *)
(*   signature : recompose : char list -> string = <fun>               *)
(*                                                                            *)
(*   paramètre(s) : une liste de caractères                                   *)
(*   résultat     : la chaîne des caractères composant la liste paramètre     *)
(*                                                                            *)
(******************************************************************************)
let recompose lc = List.fold_right (fun t q -> String.make 1 t ^ q) lc ""

let%test _ = recompose [] = ""
let%test _ = recompose [ 'a' ] = "a"
let%test _ = recompose [ 'a'; 'a' ] = "aa"
let%test _ = recompose [ 'a'; 'b' ] = "ab"
let%test _ = recompose [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f' ] = "abcdef"
