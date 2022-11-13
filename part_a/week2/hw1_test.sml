(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw1_solution.sml";

(* val test1 = is_older ((1,2,3),(2,3,4)) = true
val test1_equals = is_older ((2,3,4),(2,3,4)) = false
val test1_year_equals = is_older ((2,2,3),(2,3,4)) = true
val test1_month_equals = is_older ((2,3,3),(2,3,4)) = true
val test1_older = is_older ((2,3,5),(2,3,4)) = false

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1
val test2_none = number_in_month ([(2012,3,28),(2013,12,1)],2) = 0
val test2_all = number_in_month ([(2012,2,2),(2013,2,1)],2) = 2
val test2_null = number_in_month ([],2) = 0

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val test3_none = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[11, 10]) = 0
val test3_all = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4,12]) = 4
val test3_months_null = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[]) = 0
val test3_dates_null = number_in_months ([],[1,2,3]) = 0

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test4_none = dates_in_month ([(2012,2,28),(2013,3,1)],5) = []
val test4_all = dates_in_month ([(2012,2,28),(2013,2,1)],2) = [(2012,2,28), (2013,2,1)]
val test4_null = dates_in_month ([],2) = []

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]
val test5_none = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[11,5,6]) = []
val test5_all = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4,12]) = [(2012,2,28),(2011,3,31),(2011,4,28),(2013,12,1)]
val test5_all_different_order = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[12, 2,3,4]) = [(2013,12,1),(2012,2,28),(2011,3,31),(2011,4,28)]
val test5_month_null = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[]) = []
val test5_dates_null = dates_in_months ([],[2,3,4]) = [] 


val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"
val test6_3 = get_nth (["hi", "there", "how", "are", "you"], 3) = "how"
val test6_1 = get_nth (["hi", "there", "how", "are", "you"], 1) = "hi"
val test6_out_of_bounds = get_nth (["hi", "there", "how", "are", "you"], 12) = "Out of Bounds" 

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"
val test7_Jan = date_to_string (2013, 1, 1) = "January 1, 2013" 

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3
val test8_some_not_to_zero = number_before_reaching_sum (10, [1,2,3,2,5]) = 4
val test8_all = number_before_reaching_sum (1000, [1,2,3,4,5]) = 5
val test8_none = number_before_reaching_sum (1, [5,2,3,4,5]) = 0
val test8_null = number_before_reaching_sum (10, []) = 0 

val test9 = what_month 70 = 3

val test10 = month_range (31, 34) = [1,2,2,2]
val test10_repeat = month_range (1, 4) = [1,1,1,1]
val test10_one = month_range (1, 1) = [1]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
val test11_null = oldest([]) = NONE
val test11_day = oldest([(2012,2,28),(2011,3,31),(2011,3,28)]) = SOME (2011,3,28) *)

(* val testduplicates = remove_duplicates([1,1,1,1,1,1,1]) = [1]
val testduplicates_2 = remove_duplicates([2,3,3,3,3,4]) = [2,3,4]

val test12 = number_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,3,3,3,4]) = 3
val test12_none = number_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[11, 11, 10, 11]) = 0
val test12_all = number_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,2,2,2,4,2,3,3,12,3]) = 4
val test12_months_null = number_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[]) = 0
val test12_dates_null = number_in_months_challenge ([],[1,2,3]) = 0 *)

val test13_reasonable_1 = reasonable_date ((2013, 6, 1)) = true
val test13_reasonable_2 = reasonable_date ((2013, 2, 28)) = true
val test13_reasonable_3 = reasonable_date ((2014, 1, 1)) = true
val test13_reasonable_4 = reasonable_date ((2017, 12, 30)) = true
val test13_reasonable_leap = reasonable_date ((2020, 2, 29)) = true
val test13_notreasonable_dayzero = reasonable_date ((2013, 1, 0)) = false
val test13_notreasonable_daybig = reasonable_date ((2013, 1, 32)) = false
val test13_notreasonable_monthzero = reasonable_date ((2013, ~15, 1)) = false
val test13_notreasonable_monthbig = reasonable_date ((2013, 16, 15)) = false
val test13_notreasonable_yearzero = reasonable_date ((~2, 1, 2)) = false
val test13_notreasonable_leap = reasonable_date ((2019, 2, 29)) = false