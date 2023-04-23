(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

(**** for the challenge problem only ****)

datatype typ = Anything
| UnitT
| IntT
| TupleT of typ list
| Datatype of string
            
(**** you can put all your code here ****)

val only_capitals = List.filter (fn s => Char.isUpper(String.sub(s, 0)))
(* val only_capitals = List.filter (Char.isUpper o String.sub o (fn s => (s, 0))) *)

val test1a = only_capitals ["A","B","C"] = ["A","B","C"]
val test1b = only_capitals ["a","B","C"] = ["B","C"]

val longest_string1 = List.foldl (fn (x,acc) => if String.size(x) > String.size(acc) then x else acc) ""

val test2a = longest_string1 ["A", "bc", "C"] = "bc"
val test2b = longest_string1 ["abcd", "ef", "ghijk"] = "ghijk"
val test2c = longest_string1 [] = ""
val test2d = longest_string1 ["de", "abc", "jkl"] = "abc"

val longest_string2 = List.foldl (fn (x,acc) => if String.size(x) >= String.size(acc) then x else acc) ""

val test3a = longest_string2 ["A", "bc", "C"] = "bc"
val test3b = longest_string2 ["abcd", "ef", "ghijk"] = "ghijk"
val test3c = longest_string2 [] = ""
val test3d = longest_string2 ["de", "abc", "jkl"] = "jkl"

(* longest_string_helper has type (int * int -> bool) -> string list -> string *)
fun longest_string_helper f = List.foldl (fn (x, acc) => if f (String.size x, String.size acc) then x else acc) ""

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val test4a_1 = longest_string3 ["A", "bc", "C"] = "bc"
val test4b_1 = longest_string3 ["abcd", "ef", "ghijk"] = "ghijk"
val test4c_1 = longest_string3 [] = ""
val test4d_1 = longest_string3 ["de", "abc", "jkl"] = "abc"

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val test4a_2 = longest_string4 ["A", "bc", "C"] = "bc"
val test4b_2 = longest_string4 ["abcd", "ef", "ghijk"] = "ghijk"
val test4c_2 = longest_string4 [] = ""
val test4d_2 = longest_string4 ["de", "abc", "jkl"] = "jkl"

val longest_capitalized = longest_string1 o only_capitals

val test5a = longest_capitalized ["A","bc","C"] = "A"
val test5b = longest_capitalized [] = ""
val test5c = longest_capitalized ["Hello", "world"] = "Hello"
val test5d = longest_capitalized ["AaAa", "BBB", "CcCcCc"] = "CcCcCc"

val rev_string = String.implode o List.rev o String.explode 

val test6 = rev_string "abc" = "cba"

fun first_answer f xs =
        case xs of
            [] => raise NoAnswer
        | x::xs' => (
                case f x of
                    NONE => first_answer f xs'
                | SOME v => v
            )

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4


fun all_answers f xs =
        let
            fun helper xs acc =
                    case xs of 
                        [] => SOME acc
                    | x::xs =>
                        case f x of
                            NONE => NONE
                        | SOME lst => helper xs (acc @ lst )
        in
            helper xs []
        end


(* ('a -> 'b list option) -> 'a list -> 'b list option *)

val test8a = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8b = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = SOME []
val test8c = all_answers (fn x => if x > 3 then SOME [x] else NONE) [4,5] = SOME [4,5]


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

val count_wildcards = g (fn () => 1) (fn _ => 0)
        
val test9a_1 = count_wildcards Wildcard = 1
val test9a_2 = count_wildcards (Variable "x") = 0
val test9a_3 = count_wildcards UnitP = 0
val test9a_4 = count_wildcards (ConstP 5) = 0
val test9a_5 = count_wildcards (TupleP [Wildcard, Variable "x", Wildcard]) = 2
val test9a_6 = count_wildcards (ConstructorP ("Some", Wildcard)) = 1
val test9a_7 = count_wildcards (ConstructorP ("Some", TupleP [Wildcard, Wildcard])) = 2
val test9a_8 = count_wildcards (TupleP []) = 0

val count_wild_and_variable_lengths = g (fn () => 1) (fn x => String.size x)

val test9b_1 = count_wild_and_variable_lengths Wildcard = 1
val test9b_2 = count_wild_and_variable_lengths (Variable "x") = 1
val test9b_3 = count_wild_and_variable_lengths (UnitP) = 0
val test9b_4 = count_wild_and_variable_lengths (ConstP 5) = 0
val test9b_5 = count_wild_and_variable_lengths (TupleP [Wildcard, Variable "xyz", Wildcard]) = 5
val test9b_6 = count_wild_and_variable_lengths (ConstructorP ("Some", Wildcard)) = 1
val test9b_7 = count_wild_and_variable_lengths (ConstructorP ("Some", Variable "hello")) = 5
val test9b_8 = count_wild_and_variable_lengths (ConstructorP ("Some", TupleP [Wildcard, Variable "x", Variable "yz"])) = 4

fun count_some_var(s, p) = g (fn () => 0) (fn x => if x = s then 1 else 0) p

val test9c_1 = count_some_var ("x", Variable "x") = 1
val test9c_2 = count_some_var ("x", Variable "y") = 0
val test9c_3 = count_some_var ("x", TupleP [Variable "x", Wildcard, Variable "x"]) = 2
val test9c_4 = count_some_var ("x", ConstructorP ("Some", Variable "x")) = 1
val test9c_5 = count_some_var ("y", ConstructorP ("Some", Variable "x")) = 0
val test9c_6 = count_some_var ("x", ConstructorP ("Some", TupleP [Variable "x", Variable "y"])) = 1

fun check_pat p = 
        let 
            fun strings_from_variables p =
                    case p of 
                        Variable x => [x]
                    | TupleP ps =>  List.foldl (fn (p,i) => i @ strings_from_variables p) [] ps
                    | ConstructorP(_,p) => strings_from_variables p
                    | _ => []
            fun has_repeats xs =
                    case xs of 
                        [] => false
                    | x::xs' => List.exists (fn y => y = x) xs' orelse has_repeats xs'
        in 
            not (has_repeats (strings_from_variables p))
        end

val test10_1 = check_pat (Variable "x") = true
val test10_2 = check_pat (TupleP [Variable "x", Variable "y"]) = true
val test10_3 = check_pat (TupleP [Variable "x", Variable "x"]) = false
val test10_4 = check_pat (ConstructorP ("None", Wildcard)) = true
val test10_5 = check_pat (ConstructorP ("Some", Variable "x")) = true 


fun match (v, p) =
        case (v, p) of 
            (_, Wildcard) => SOME []
        | (v, Variable s) => SOME [(s, v)]
        | (Unit, UnitP) => SOME []
        | (Const c, ConstP cp) => if c = cp then SOME [] else NONE
        | (Tuple t, TupleP tp) => 
            if List.length t = List.length tp 
            then all_answers (fn (vt, pt) => match (vt, pt)) (ListPair.zip (t, tp))
            else NONE
        | (Constructor (s2, cv), ConstructorP (s1, cp)) => 
            if s1 = s2 then match(cv, cp)
            else NONE
        | _ => NONE


val test11_1 = match(Const 1, Wildcard) = SOME []
val test11_2 = match(Const 1, Variable "x") = SOME [("x", Const 1)]
val test11_3 = match(Unit, UnitP) = SOME []
val test11_4 = match(Const 17, ConstP 17) = SOME []
val test11_5 = match(Tuple [Const 1, Const 2, Const 3], TupleP [ConstP 1, Wildcard, Variable "x"]) = SOME [("x", Const 3)]
val test11_6 = match(Constructor ("MyConst", Tuple [Const 1, Const 2]), ConstructorP ("MyConst", TupleP [Wildcard, Variable "x"])) = SOME [("x", Const 2)]
val test11_7 = match(Const 1, ConstructorP ("MyConst", Variable "x")) = NONE

fun first_match v ps =
        SOME (first_answer (fn p => match(v, p)) ps)
        handle NoAnswer => NONE

val test12_1 = first_match Unit [UnitP] = SOME []
val test12_2 = first_match (Const 1) [Wildcard, ConstP 2] = SOME []
val test12_3 = first_match (Tuple [Const 1, Const 2]) [TupleP [ConstP 1, Variable "x"], TupleP [Variable "y", ConstP 2]] = SOME [("x", Const 2)]
val test12_4 = first_match (Constructor ("MyConst", Tuple [Const 1, Const 2])) [ConstructorP ("MyConst", TupleP [Wildcard, Variable "x"])] = SOME [("x", Const 2)]
val test12_5 = first_match (Constructor ("MyConst", Tuple [Const 1, Const 2])) [ConstructorP ("OtherConst", TupleP [Wildcard, Variable "x"])] = NONE
