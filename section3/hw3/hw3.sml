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

val longest_string1 = List.foldl (fn (x,acc) => if String.size(x) > String.size(acc) then x else acc) ""

val longest_string2 = List.foldl (fn (x,acc) => if String.size(x) >= String.size(acc) then x else acc) ""

(* longest_string_helper has type (int * int -> bool) -> string list -> string *)
fun longest_string_helper f = List.foldl (fn (x, acc) => if f (String.size x, String.size acc) then x else acc) ""

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode 

fun first_answer f xs =
        case xs of
            [] => raise NoAnswer
        | x::xs' => (
                case f x of
                    NONE => first_answer f xs'
                | SOME v => v
            )

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

val count_wild_and_variable_lengths = g (fn () => 1) (fn x => String.size x)

fun count_some_var(s, p) = g (fn () => 0) (fn x => if x = s then 1 else 0) p

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

fun first_match v ps =
        SOME (first_answer (fn p => match(v, p)) ps)
        handle NoAnswer => NONE
