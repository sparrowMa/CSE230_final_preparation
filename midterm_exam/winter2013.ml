(* Win 2013 *)

(* 1a *)

type 'a maybe =
| None
| Some of 'a
;;


let first f l =
	let base = None in
	let fold_fn acc elmt =
		if (acc != None) then acc	
		else if (f elmt) then (Some elmt)
			else None
	in
List.fold_left fold_fn base l


more ocaml way....

let first f l =
	let base = None in
	let fold_fn acc elmt =
		match acc with
		| Some a -> Some a
		| None -> if (f elmt) then (Some elmt) else None
	in
List.fold_left fold_fn base l




let even x = (x mod 2 = 0)

first even [1;3;4;5;7;9]

aaaaaaaaaaaaaaaaaa


(* 2.a *)

let rec zip l1 l2 =
	match (l1,l2) with
	| ([],l2) -> []
	| (l1,[]) -> []
	| ((h1::t1),(h2::t2)) -> [(h1,h2)] @ (zip t1 t2)

zip [1;2;3] [5;6;7]

zip ['a';'b';'c'] [1;2;3]

zip ['a'] [1;2;3]

(* 2.b *)

let map2 f l1 l2 =
	let fmap elmt = let (ele1,ele2)=elmt in (f ele1 ele2)
in List.map fmap (zip l1 l2)


(* 2.c *)
let map3 f l1 l2 l3 = 
	let fmap elmt = let ((e1,e2),e3) = elmt in (f e1 e2 e3)
in List.map fmap (zip (zip l1 l2) l3)

(* !!!! Be careful about the type of zip (zip l1 l2) l3
 *)
(* 3. *)

let rec unzip l =
	match l with
	| [] -> ([], [])
	| h::t -> let (h1,h2) = h in
			  let (l1,l2) = (unzip t) in
			  ([h1] @ l1, [h2] @ l2)

(* ahhhhhhhhhh!! return type!! *)

unzip [(1,2);(3,4);(5,6)]

unzip [('a',1);('b',2)]







