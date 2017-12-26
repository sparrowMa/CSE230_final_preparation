(* Win 2012 mid-term
 *)

(* 1a. *)

let rec split l =
	let len = List.length l in
	let base = (0,[],[]) in
	let fold_fn (i,l1,l2) elmt= 
		if (i<(len/2)) then ((i+1), l1@[elmt],l2)
		else ((i+1),l1,l2@[elmt])
		in
	let (_,l1,l2) = List.fold_left fold_fn base l in
	(l1,l2)
;;

(* 1b. *)
let rec merge l1 l2 =
	match (l1,l2) with
	|([],l) -> l
	|(l,[]) -> l
	|((h1::t1),(h2::t2)) ->
		if (h1 <= h2) then [h1] @ (merge t1 l2) 
	else [h2] @ (merge l1 t2)
;;


(* 1c. *)
(* let rec merge_sort l =
	match l with
	| [] -> []
	| _ -> let (l1,l2) = split l in
			match (l1,l2) with
			|([],l) -> l
			|((h1::[]),(h2::[])) -> merge l1 l2
			|((h1::[]),l) -> merge l1 (merge_sort l)
			| _ -> merge (merge_sort l1) (merge_sort l2)

cleaner version..

let rec merge_sort l =
	match l with
	| [] -> []
	| (h::[]) -> [h]
	| (h::t) -> let (l1,l2) = split l in
				merge (merge_sort l1) (merge_sort l2)
 *)

let rec merge_sort l =
	match l with
	| [] -> []
	| [h] -> [h]
	| _ -> let (l1,l2) = split l in
		   let sl1 = merge_sort l1 in
		   let sl2 = merge_sort l2 in
		   merge sl1 sl2
;;





merge_sort [2;10;3;2;1]




(* 2. *)

let explode s =
  let rec exp i l =
   if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;

let implode l =
 let res = String.create (List.length l) in
 let rec imp i = function
  | [] -> res
  | c :: l -> res.[i] <- c; imp (i + 1) l in
 imp 0 l;;


(* let replace s =
	let fmap = fun a -> if (a='-') then ' ' else a
in implode(List.map fmap (explode s))

 *)

let replace s =
	let fmap a = if (a='-') then ' ' else a
in implode(List.map fmap (explode s))


(* replace "a-b-c";; *)


(* 3a. *)

let app l x =
	let fmap finl = finl x
in List.map fmap l
;;

(* 3b *)
let [f1;f2] = app [(=);(<)] 2;;








