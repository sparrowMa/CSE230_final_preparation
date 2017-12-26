(* spring 2013 *)


(* 1.a *)
let length l =
	let base = 0 in
	let fold_fn acc elmt = (acc + 1) in
List.fold_left fold_fn base l
;;


let length l =
	let base = 0 in
	let fold_fn acc elmt = acc + 1 in
List.fold_left fold_fn base l
;;

(* parenthesis is unneccesary *)

(* 1.b *)
let remove l x =
	let base = [] in
	let fold_fn acc elmt = if (elmt=x) then acc else (acc @ [elmt]) in
List.fold_left fold_fn base l



(* 2.a *)
let rec ith l i d=
	match l with
	| [] -> d
	| h::t -> match i with
		| 0 -> h
		| _ -> (ith t (i-1) d)

ith ["a";"b";"c";"d"] 0 " "

ith [["a"];["b"]] 0 [" "]

(* 2.b *)
let rec update l i n =
	match l with
	| [] -> l
	| h::t -> match i with
		| 0 -> [n] @ t
		| _ -> [h] @ (update t (i-1) n)

update ["a";"b";"c";"d"] 0 "ZZZ"


(* 2.c *)
(* let rec update2 l i n d =
	match l with
	| [] -> match i with
		| 0 -> [n]
		| _ -> ([d] @ (update2 [] (i-1) n d))
	| h::t -> match i with
		| 0 -> [n] @ t
		| _ -> [h] @ (update2 t (i-1) n d)
 *)
(* seems like an illegal expression *)

let rec update2 l i n d =
	match l with
	| [] -> if (i=0) then [n] else [d] @ (update2 [] (i-1) n d)
	| h::t -> match i with
		| 0 -> [n] @ t
		| _ -> [h] @ (update2 t (i-1) n d)

update2 ["a";"b";"c";"d"] 6 "ZZZ" " "

(* 3 *)
let categorize f l =
	let base = [] in
	let fold_fn acc elmt=
		update2 acc (f elmt) ( (ith acc (f elmt) []) @ [elmt] ) []
in
List.fold_left fold_fn base l



(* my version of categorize *)

let categorize f l =
	let base = [] in
	let fn acc elmt = 
		let binNum = f elmt in
		let binList = ith acc binNum [] in
		update2 acc binNum (binList@[elmt]) []
in
List.fold_left fn base l

(* 
	here acc (aka the format of the final result) is: a list list.
	here binList can be [] or have some value.
	It doesn't matter to do binList@[elmt] whether or not it's empty.
*)


let f i = if i < 0 then 0
  else (if i < 10 then 1
  else (if i < 20 then 2 else 3))
;;

categorize f [1;2;-3;15;7;30;-1;22;33;14;105]









