(* fall 2014 *)

(* 1.a *)
type expr =
| Const of int
| Var of string
| Op of string * expr * expr

let rec rename_var e n1 n2 =
	match e with
	| Const i -> Const i
	| Var s -> Var(if (s = n1) then n2 else s)
	| Op (s,e1,e2) -> Op (s, (rename_var e1 n1 n2), (rename_var e2 n1 n2))

rename_var (Op ("+", Var "a", Const 4)) "a" "b"


(* 1.b *)
let to_str e=
	let rec str_helper e top_level =
		match e with
		| Const i -> string_of_int i
		| Var s -> s
		| Op (s,e1,e2) -> if top_level then 
			((str_helper e1 false) ^ s ^ (str_helper e2 false))
			else ("(" ^ (str_helper e1 false) ^ s ^ (str_helper e2 false) ^ ")")
	in str_helper e true



(* 2. *)
let average_if f l =
	let base = (0,0) in
	let fold_fn (num,sum) elmt =
		if (f elmt) then ((num+1),(sum+elmt))
		else (num,sum)
	in
	let (num, res) = List.fold_left fold_fn base l in
	(res/num)



average_if even [1;2;3;4;5]


(* 3.a *)

let length_2 l =
	List.fold_left (+) 0 (List.map (List.length) l)

length_2 [[1;2;3];[4;6]]

length_2 [[];[];[]]

(* 3.b *)

let length_3 l =
	List.fold_left (+) 0 (List.map length_2 l)

length_3 [[[1;2;3]];[[4;6];[7;8]]]


(* 4. *)
let f1 = List.map (fun x -> 2*x)

f1 [1;2;3;4];;

let f2 = List.fold_left (fun x y -> (y+2)::x) [];;
f2 [3;5;7;9];; 


let f3 = List.fold_left (fun x y -> x@[3*y]) [];;
f3 [1;3;6];; 

(* This is going to get harder now... *)
let f = List.fold_left (fun x y -> y x);;

f 1 [(+) 1; (-) 2];;


f "abc" [(^) "zzz"; (^) "yyy"];; 



f [1;2;3] [f1;f2;f3];; 




