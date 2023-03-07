
type 'a arbre_naire = Noeud of 'a * 'a arbre_naire list

let cons el list_arbre_naire =
  Noeud (el, list_arbre_naire )

let racine (Noeud (r,_)) = r

let fils (Noeud (_,fils)) = fils

let rec map_arbre_naire f (Noeud (r, list_arbre_naire)) =
  Noeud (f r, List.map (map_arbre_naire f) list_arbre_naire)

let rec fold_arbre_naire f e (Noeud (r,list_arbre_naire)) =
  f r (List.map (fold_arbre_naire f ) list_arbre_naire);;

let rec cardinal arb =
  fold_arbre_naire (fun _ lcf -> (1 + List.fold_left (+) 0 lcf)) 0 arb;;

