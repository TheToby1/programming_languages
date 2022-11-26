(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw3_sol.sml";

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1_harder = only_capitals ["Adfs","Basda","casda", "Ksdsd"] = ["Adfs","Basda","Ksdsd"]

val test2 = longest_string1 ["A","bc","C"] = "bc"
val test2_empty = longest_string1 [] = ""
val test2_equal = longest_string1 ["A","bc","Cd"] = "bc"

val test3 = longest_string2 ["A","bc","C"] = "bc"
val test3_empty = longest_string2 [] = ""
val test3_equal = longest_string2 ["A","bc","Cd"] = "Cd"

val test4a = longest_string3 ["A","bc","C"] = "bc"
val test4a_empty = longest_string3 [] = ""
val test4a_equal = longest_string3 ["A","bc","Cd"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"
val test4b_empty = longest_string4 [] = ""
val test4b_equal = longest_string4 ["A","bc","Cd"] = "Cd"

val test5 = longest_capitalized ["A","bc","C"] = "A"
val test5_empty = longest_capitalized [] = ""

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8_notnone = all_answers (fn x => if x > 1 then SOME [x] else NONE) [2,3,4,5,6,7] = SOME [7,6,5,4,3,2]
val test8_none = all_answers (fn x => if x > 1 then SOME [x] else NONE) [0,3,4,5,6,7] = NONE

val test9a = count_wildcards Wildcard = 1
val test9a_more = count_wildcards (TupleP [Wildcard, (TupleP [Wildcard, Variable ""]), Wildcard, (Variable "")]) = 3
val test9a_none = count_wildcards (Variable "") = 0

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

val test9c = count_some_var ("x", Variable("x")) = 1

val test10 = check_pat (Variable("x")) = true
val test10_not = check_pat ((TupleP [Variable("x"), Variable("x")])) = false
val test10_more = check_pat (TupleP [Wildcard, (TupleP [Wildcard, Variable "x"]), Wildcard, (Variable "y")]) = true
val test10_more_not = check_pat (TupleP [Wildcard, (TupleP [Wildcard, Variable "y"]), Wildcard, (Variable "y")]) = false

val test11 = match (Const(1), UnitP) = NONE
val test11_simplematch = match (Unit, UnitP) = SOME []
val test_fail = match (Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4))],TupleP[Wildcard,Wildcard]) = NONE

val test12 = first_match Unit [UnitP] = SOME []
val test12_notfirst = first_match Unit [ConstP(1), Variable("hi")] = SOME [("hi", Unit)]
val test12_none = first_match Unit [ConstP(1)] = NONE

val testchallenge = typecheck_patterns ([], [ConstP(10), Variable("a")]) = SOME IntT
val testchallenge_fail = typecheck_patterns ([], [ConstP 10, Variable "a", ConstructorP("SOME",Variable "x")]) = NONE
val testchallenge_3 = typecheck_patterns ([], [TupleP [Variable("a"), ConstP(10), Wildcard], TupleP [Variable("b"), Wildcard, ConstP(11)], Wildcard]) = SOME (TupleT[Anything,IntT,IntT])
val testchallenge_datatype = typecheck_patterns ([("Red","color",UnitT),("Green","color",UnitT),("Blue","color",UnitT)], [ConstructorP("Red", UnitP), Wildcard]) = SOME (Datatype "color")
val testchallenge_datatype2 = typecheck_patterns ([("Sedan","auto", Datatype "color"),("Truck","auto",TupleT[IntT, Datatype "color"]),("SUV","auto",UnitT)], [ConstructorP("Sedan", Variable "a"), ConstructorP("Truck", TupleP[Variable "b", Wildcard]), Wildcard]) = SOME (Datatype "auto")
val testchallenge_datatype3 = typecheck_patterns ([("Empty","list",UnitT),("List","list",TupleT[Anything, Datatype "list"])], [ConstructorP("Empty",UnitP),ConstructorP("List",TupleP[ConstP 10, ConstructorP("Empty",UnitP)]), Wildcard]) = SOME (Datatype "list")

val testchallenge_example = typecheck_patterns ([], [TupleP[Wildcard, Wildcard], TupleP[Wildcard, TupleP[Wildcard,Wildcard]]]) = SOME (TupleT[Anything, TupleT[Anything, Anything]])
