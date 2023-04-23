(* Homework1 Simple Test *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = is_older ((1,2,3),(2,3,4)) = true

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val test3_2 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,2,28)],[2,3]) = 3

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test4_2 = dates_in_month ([(2012,2,28),(2013,12,1),(2013,2,12)],2) = [(2012,2,28), (2013,2,12)]

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3
val test8_2 = number_before_reaching_sum (11, [1,2,3,4,5,6]) = 4

val test9 = what_month 70 = 3
val test9_2 = what_month 25 = 1
val test9_3 = what_month 50 = 2

val test10 = month_range (31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31) 

val test12_1 = remove_duplicates([1, 2, 1, 2, 3]) = [1, 2, 3]
val test12_2 = number_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,3,4]) = 3
val test12_3 = number_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,2,28)],[2,2,3]) = 3 
val test12_4 = dates_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test13_1 = reasonable_date (2023, 3, 10) = true
val test13_2 = reasonable_date (2023, 2, 29) = false
val test13_3 = reasonable_date (~100, 5, 20) = false