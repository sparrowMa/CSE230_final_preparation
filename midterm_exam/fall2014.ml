(* Fall 2014 midterm
 *)


(* 1. *)
(* datatype for representing arithmetic expressions with constants, 
variables and binary operations. *)


type expr =
| Const of int
| Var of string
| Op of string * expr * expr;;


(* b *)


let to_str e =
	let rec str_helper e top_level =
	match e with
	| Var s -> s
	| Const i -> string_of_int(i)
    | Op (s,e1,e2) -> 
    	let lefts = str_helper e1 false in 
    	let rights = str_helper e2 false in
    	match top_level with
    	| true -> lefts ^ s ^ rights
    	| false -> "(" ^ lefts ^ s ^ rights ^ ")"
in
str_helper e true;;


# to_str (Op ("+", Var "a", Const 4));;
- : string = "a+4"
# to_str (Op ("+", Const 10, Op ("+", Const 10, Var "b")));;
- : string = "10+(10+b)"
# to_str (Op ("+", Op ("*", Var "x", Var "y"), Op ("-", Var "x", Var "z")));;
- : string = "(x*y)+(x-z)"


