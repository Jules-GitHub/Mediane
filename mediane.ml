let mediane5 liste =
  let l = List.sort compare liste in
  List.nth l (((List.length liste)-1)/2)
;;

let divise5 liste =
  let rec creeListes liste n sousListe = match n, liste with
  | k, t::q -> if (k=4) then (
    (t::sousListe)::(creeListes q 0 [])
  ) else (
    creeListes q (k+1) (t::sousListe)
  )
  | _, [] -> if (sousListe <> []) then [sousListe] else []
  in creeListes liste 0 []
;;

(* Question 3
Dans chaque sous-liste, la médiane est supérieur ou égale (resp inférieure ou égale) à 3 valeurs,
c'est-à-dire 60% des valeurs de la liste.
Quand on prend la médiane des médiane, elle est inférieure ou égale à la moitié des médianes et donc à au moins 
50%*60% = 30% des valeurs totales *)

let partition liste pivot = List.partition (fun x -> x > pivot) liste;;

let liste_medianes liste = List.map (fun l -> mediane5 l) (divise5 liste);;

let rec mediane liste = match List.length liste with
| n when n <= 5 -> mediane5 liste
| _ -> mediane (liste_medianes liste);;

let rec k_plus_petit liste k = match List.length liste with
| 0 -> failwith "Taille de liste incorrecte"
| 1 -> List.hd liste
| _ -> let m = mediane liste in
  if (List.for_all (fun x -> x = m) liste) then m
  else (
    let l1, l2 = partition liste m in
    let n2 = List.length l2 in
    if (k <= n2) then (k_plus_petit l2 k)
    else (k_plus_petit l1 (k-n2))
  )
;;

let vraie_med liste = k_plus_petit liste ((List.length liste)/2);;

let l = [2;9;13;5;1;12;15;17;18;4;3;7;8;10;20;19;11;6;14;16];;
let l2 = [2;4;2;6;7;4];;