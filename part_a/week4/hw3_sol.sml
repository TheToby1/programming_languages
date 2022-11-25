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
fun longest_string_helper comparison xs = List.foldl (fn (x,y) =>
														if comparison(String.size(x),String.size(y)) 
														then x 
														else y) 
														"" xs

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

(* 5 *)
val longest_capitalized = longest_string3 o only_capitals

(* 6 *)
val rev_string = String.implode o rev o String.explode

(* 7 *)
fun first_answer aToB xs = 
	case xs
	of [] => raise NoAnswer
	| x::xs' => case aToB x
				of SOME y => y
				| NONE => first_answer aToB xs'

(* 8 *)
fun all_answers aToBList xs =
	let
		fun concat_through_somes (xs, acc) =
			case xs
			of [] => SOME acc
			| x::xs' => case aToBList x
						of SOME y => concat_through_somes (xs', y@acc)
						| NONE => NONE
	in
		concat_through_somes (xs, [])
	end

	
(* 9a *)
val count_wildcards = g (fn _ => 1) (fn _ => 0)

(* 9b *)
val count_wild_and_variable_lengths = g (fn _ => 1) (fn x => String.size(x))

(* 9c *)
fun count_some_var (s, p) = g (fn _ => 0) (fn x => if s = x then 1 else 0) p

(* 10 *)
fun check_pat p =
	let
		fun get_string_vars p acc = 
			case p 
			of Variable x        => x :: acc
			| TupleP ps         => (List.foldl (fn (p,i) => (get_string_vars p []) @ i) [] ps) @ acc
			| ConstructorP(_,p) => get_string_vars p acc
			| _                 => acc
		fun check_duplicates xs =
			case xs
			of [] => true
			| x::xs' => if List.exists (fn y => x = y) xs'
						then false
						else check_duplicates xs'
		val string_vars = get_string_vars p []
	in
		check_duplicates string_vars
	end

(* 11 *)
fun match (v, p) = 
	case (v, p)
	of (_, Wildcard) => SOME []
	| (_, Variable s) => SOME [(s, v)]
	| (Unit, UnitP) => SOME []
	| (Const x, ConstP y) => if x = y
								then SOME []
								else NONE
	| (Tuple vs, TupleP ps) => if List.length vs = List.length ps
								then all_answers match (ListPair.zip (vs, ps))
								else NONE			
	| (Constructor (s1, v), ConstructorP (s2,p)) => if s1 = s2
														then match (v, p)
														else NONE
	| _ => NONE

fun first_match v ps =
	(SOME (first_answer match (ListPair.zip (List.tabulate ((List.length ps), (fn _ => v)), ps)))) 
		handle NoAnswer => NONE
