(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw2_sol.sml";

val test1 = all_except_option ("string", ["string"]) = SOME []
val test1_remaining_end = all_except_option ("string", ["string", "Test"]) = SOME ["Test"]
val test1_remaining_start = all_except_option ("string", ["Test", "string"]) = SOME ["Test"]
val test1_remaining_both = all_except_option ("string", ["Test", "string"]) = SOME ["Test"]
val test1_none = all_except_option ("string", ["Test", "Lol"]) = NONE


val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test2_example = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]

val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test3_example = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test5 = card_color (Clubs, Num 2) = Black

val test6 = card_value (Clubs, Num 2) = 2

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test7_lots = remove_card ([(Clubs, Num 2), (Spades, Num 2), (Hearts, Ace)], (Hearts, Ace), IllegalMove) = [(Clubs, Num 2), (Spades, Num 2)]
val test7_exception = (remove_card ([(Hearts, Ace)], (Hearts, King), IllegalMove) handle IllegalMove => [(Hearts, Num 0)]) = [(Hearts, Num 0)]

val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test8_false = all_same_color [(Hearts, Ace), (Spades, Ace)] = false

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6
val test11_big = officiate ([(Hearts, Num 2),(Clubs, Num 9),(Spades, Num 4),(Diamonds, Ace),(Clubs, Num 2),(Diamonds, Num 7),(Hearts, King)],[Draw, Draw, Draw, Discard (Clubs, Num 9), Draw, Draw, Draw], 26) = 0
val test11_early_end = officiate ([(Hearts, Num 2),(Clubs, Num 9)],[Draw, Draw, Draw, Draw, Draw, Draw, Draw, Draw], 10) = 3
val test11_early_end_color = officiate ([(Hearts, Num 2),(Diamonds, Num 9)],[Draw, Draw, Draw, Draw, Draw, Draw, Draw, Draw], 10) = 1
val test11_early_end_overdraw = officiate ([(Hearts, Num 2),(Clubs, Num 9),(Spades, Num 4),(Diamonds, Ace),(Clubs, Num 2),(Diamonds, Num 7),(Hearts, King)],[Draw, Draw, Draw, Draw, Draw, Discard (Clubs, Num 9), Draw], 26) = 6
val test11_illegal = (officiate ([(Hearts, Num 2),(Diamonds, Num 9)],[Draw, Draw, Discard (Clubs, Num 9), Draw, Draw, Draw, Draw, Draw, Draw], 20) handle IllegalMove => ~1) = ~1
val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw], 42) = 3
val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true) 


val testChallenge_a1 = officiate_challenge ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw], 40) = 3
val testChallenge_a2 = officiate_challenge ([(Clubs,Ace),(Hearts,Ace),(Clubs,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw], 40) = 6

val test_card_of_value = get_card_of_value ([(Spades,Num 7),(Hearts,King),(Clubs,Ace),(Diamonds,Num 2)], 10) = (Hearts,King)
val testChallenge_b1 = careful_player ([(Spades,Num 7),(Hearts,King),(Clubs,Ace),(Diamonds,Num 2)], 18) = [Draw, Draw, Discard (Hearts, King), Draw]
val testChallenge_b2 = careful_player ([(Spades,Num 7)], 11) = [Draw]
val testChallenge_b3 = careful_player ([(Diamonds,Num 2),(Clubs,Ace)], 11) = [Draw, Discard (Diamonds,Num 2), Draw]
val testChallenge_b4 = careful_player ([(Diamonds,Num 2)], 15) = [Draw, Draw]

