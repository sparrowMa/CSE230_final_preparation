(* 2013 fall *)

(* 1.a *)
let count l x=
	let base = 0 in
	let fold_fn acc elmt= if (elmt=x) then (acc+1) else acc in
List.fold_left fold_fn base l

count [1;2;3;4;5] 10

(* 1.b *)
let make_palindorme l =
	let base = [] in
	let fold_fn acc elmt = [elmt] @ acc @ [elmt] in
List.fold_left fold_fn base l

make_palindorme [1;2]


(* 2.a *)

(* let fold_2 f b l =
	let base = (0,[]) in
	let fold_fn acc elmt = 
		let (index, resl) = acc in
		((index+1), resl @ (f resl elmt index))
	in
let (_,res) = List.fold_left fold_fn base l
in res


let fold_2 f b l =
	let base = (0,[]) in
	let fold_fn acc elmt = 
		let (index, resl) = acc in
		let (_, newl) = (f acc elmt index) in
		((index+1), resl @ newl)
	in
let (_,res) = List.fold_left fold_fn base l
in res *)


let fold_2 f b l =
	let base = (b,0) in
	let fold_fn (acc,index) elmt = 
		((f acc elmt index),(index+1))
	in
let (res,_) = List.fold_left fold_fn base l
in res



let f_string_of_int a e i =
	a ^ " " ^ (string_of_int e) ^ " " ^ (string_of_int i)
;;

f_string_of_int "" 2 3

# f_string_of_int " " 2 3;;
- : string = "  2 3"


# fold_2 f_string_of_int " " [10;0;4];;
- : string = "  10 0 0 1 4 2"


(* when don't know the type of l, just use what is provided in the funtion head!
Like b here
 *)

(* 2.b *)
let ith l i d =
	let base = d in
	let fold2_fn acc elmt index = 
		match acc with 
		| d -> if (index=i) then elmt else acc
		| _ -> acc
	in
fold_2 fold2_fn base l

ith ["a";"b";"c";"d"] 0 " "


(* 3.a *)

type 'a fun_tree=
| Leaf of ('a -> 'a)
| Node of ('a fun_tree) * ('a fun_tree)
;;

let rec apply_all t x =
	match t with
	| Leaf leaff -> leaff x
	| Node (n1,n2) -> apply_all n2 (apply_all n1 x)
;;

# let f1 x = x+1;;
val f1 : int -> int = <fun>
# let f2 x = x* 2;;
val f2 : int -> int = <fun>
# let f3 x = x + 3;;
val f3 : int -> int = <fun>
# let t = Node(Leaf f1, Node(Leaf f2, Leaf f3));;
val t : int fun_tree = Node (Leaf <fun>, Node (Leaf <fun>, Leaf <fun>))
# apply_all t 0;;
- : int = 5



let f1 = (+) 1;;
let f2 = (-) 2;;
let f3 = (+) 3;;
let t = Node(Node(Leaf f1, Leaf f2), Leaf f3);;
apply_all t 0;;


let f1 = (^) "a";;

let f2 x = x ^ "b";;

let f3 x = x ^ "ab";;

let t = Node(Leaf f1, Node(Leaf f2, Leaf f3));;

apply_all t "123";;










let f1 = List.fold_left (fun x y -> (y*2)::x) [];;

let f2 = List.fold_left (fun x y -> x@[y]) [];;

let t = Node(Node(Leaf f1, Leaf f1), Node(Leaf f1, Leaf f2));;

apply_all t [1;2;3];;




(* 3.c *)

let rec compose t1 t2 =
	match (t1,t2) with
	|(Leaf l1, Leaf l2) -> fun x -> Leaf(l1 (l2 x))
	|(Node n1, Node n2)->
		let (Node f1,Node f2) = Node n1 in
		let (Node f3,Node f4) = Node n2 in
		Node((compose f1 f3), (compose f2 f4))

 
let plus x = x + 1

plus 3















