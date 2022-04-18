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

let partition liste pivot = List.partition (fun x -> x <= pivot) liste;;

let rec k_plus_petit liste k = match List.length liste with
| 0 -> failwith "Taille de liste incorrecte"
| 1 -> List.hd liste
| n -> 
  let pivot = 
    if (n <= 5) then (
      mediane5 liste
    ) else (
      k_plus_petit (List.map (fun l -> mediane5 l) (divise5 liste)) (n/(2*5))
    )
  in
  if (List.for_all (fun x -> x = pivot) liste) then pivot
  else (
    let l1, l2 = partition liste pivot in
    let n1 = List.length l1 in
    if (k <= n1) then (k_plus_petit l1 k)
    else (k_plus_petit l2 (k-n1))
  )
;;

let rec tri_rapide liste = match List.length liste with
| 0 -> []
| 1 -> liste
| n -> let pivot = k_plus_petit liste (n/2) in
  if (List.for_all (fun x -> x = pivot) liste) then liste
  else (
    let l1, l2 = partition liste pivot in
    (tri_rapide l1) @ (tri_rapide l2)
  )
;;

let l = [2;9;13;5;1;12;15;17;18;4;3;7;8;10;20;19;11;6;14;16];;
let l2 = [2;4;2;6;7;4];;

tri_rapide l;;