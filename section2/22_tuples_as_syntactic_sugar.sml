(* records are like tuples with user-defined field names
   conversely, tuples are just records with names 1, 2, ..., n
   the only real difference is "by position" vs. "by name"
*)

val a_pair = (3 + 1, 4 + 2)
(* val a_pair = (4,6) : int * int *)

val a_record = {second= 4 + 2, first= 3 + 1}
(* val a_record = {first=4,second=6} : {first:int, second:int} *)

(* actually, tuples *are* just records with names 1, 2, ..., n and
special "by position" syntax -- our first example of "syntactic sugar" *)

val another_pair = {2= 5, 1= 6}
(* val another_pair = (6,5) : int * int *)

val sum = #1 a_pair + #2 another_pair
(* val sum = 9 : int *)

val x = {3="hi", 1=true};
(* val x = {1=true,3="hi"} : {1:bool, 3:string} *)

val y = {3="hi", 2=3+2, 1=true};
(* val y = (true,5,"hi") : bool * int * strin *)
