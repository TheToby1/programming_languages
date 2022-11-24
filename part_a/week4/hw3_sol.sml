(* Coursera Programming Languages, Homework 3, Provided Code *)
(* Edited for entire solution by TB *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* 1 *)
(* It might be nicer to seperate out anonymous function to a separate 
function called isFirstCharCaps, but the one liner felt nice*)
val only_capitals = List.filter (fn x => Char.isUpper (String.sub (x, 0)))

(* 2 *)
(* One line made it hard to read although it technically is *)
val longest_string1 = 
	List.foldl (fn (x, y) => 
					if String.size(x) > String.size(y) 
					then x 
					else y) 
				""

(* 3 *)
val longest_string2 = 
	List.foldl (fn (x, y) => 
					if String.size(x) >= String.size(y) 
					then x 
					else y) 
				""

(* 4 I could pull out the anonymous functions from above and more easily re-use them below
but I didn't want to edit the work I'd already done *)
fun longest_string_helper comparison xs = List.foldl comparison "" xs

val longest_string3 = longest_string_helper (fn (x, y) => 
												if String.size(x) > String.size(y) 
												then x 
												else y)

val longest_string4 = longest_string_helper (fn (x, y) => 
												if String.size(x) >= String.size(y) 
												then x 
												else y)

(* 5 *)
val longest_capitalized = longest_string3 o only_capitals

(* 6 *)
val rev_string = String.implode o rev o String.explode

(* 7 *)