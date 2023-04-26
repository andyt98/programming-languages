(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
        s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (mystring, los) =
        case los of
            [] => NONE
        | s::los' => 
            if same_string(s, mystring)
            then SOME los'
            else
                let val rest = all_except_option(mystring, los')
                in
                    case rest of 
                        NONE => NONE
                    | SOME l => SOME(s::l)
                end


val test1 = all_except_option("string", ["string"]) = SOME []
val test1_2 = all_except_option("apple", ["banana", "orange", "apple", "kiwi"]) = SOME ["banana", "orange", "kiwi"]
val test1_3 = all_except_option("apple", ["banana", "orange", "kiwi"]) = NONE


fun get_substitutions1(lolos, mystring) =
        case lolos of
            [] => []
        | los::lolos' =>
            let
                val subst_in_first_list = all_except_option(mystring, los)
            in
                case subst_in_first_list of
                    NONE => get_substitutions1(lolos', mystring)
                | SOME l => l @ get_substitutions1(lolos', mystring)
            end


val test2 = get_substitutions1([["foo"],["there"]], "foo") = []
val test2_2 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]
val test2_3 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff") = ["Jeffrey","Geoff","Jeffrey"]



fun get_substitutions2(lolos, mystring) =
        let
            fun helper(lolos, mystring, rsf) =
                    case lolos of 
                        [] => rsf
                    | los::lolos' =>
                        let
                            val subst_in_first_list = all_except_option(mystring, los)
                        in 
                            case subst_in_first_list of 
                                NONE => helper(lolos', mystring, rsf)
                            | SOME l => helper(lolos', mystring, rsf @ l)
                        end
        in
            helper(lolos, mystring, [])
        end

val test3 = get_substitutions2([["foo"],["there"]], "foo") = []
val test3_2 = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]
val test3_3 = get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff") = ["Jeffrey","Geoff","Jeffrey"] 


fun similar_names (lolos, {first=f, middle=m, last=l}) =
        let
            val alternative_first_names = get_substitutions2(lolos, f)
            val initial_list = [{first=f, middle=m, last=l}]
            fun make_names_from_strings (strings, rsf) =
                    case strings of
                        [] => rsf
                    | s::strings' => make_names_from_strings (strings', rsf @ [{first=s, middle=m, last=l}])
        in
            make_names_from_strings(alternative_first_names, initial_list)
        end


    
val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
        {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test4_1 = similar_names ([], {first="Fred", middle="W", last="Smith"}) = [{first="Fred", last="Smith", middle="W"}]

val test4_2 = similar_names([["Rob","Robert"],["Bob","Bobby"],["Roberto"]], {first="Bob", middle="A", last="Jones"}) =
    [{first="Bob", middle="A", last="Jones"}, {first="Bobby", middle="A", last="Jones"}]


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color(card) =
        case card of 
            (Spades, _) => Black
        | (Clubs, _) => Black
        | (Diamonds, _) => Red
        | (Hearts, _) => Red

val test5 = card_color(Clubs, Num 2) = Black

fun card_value(card) = 
        case card of 
            (_, Num n) => n
        | (_, Ace) => 11
        | _ => 10

val test6 = card_value(Clubs, Num 2) = 2
val test6_2 = card_value(Clubs, Ace) = 11
val test6_3 = card_value(Clubs, King) = 10

fun remove_card(cs, c, e) =
        case cs of
            [] => raise e
        | card::cs' => if card = c
            then cs'
            else card::remove_card(cs', c, e)

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test7_1 = remove_card ([(Spades, Num 5), (Diamonds, Jack), (Clubs, Ace)], (Diamonds, Jack), IllegalMove) = [(Spades, Num 5), (Clubs, Ace)]
val test7_2 = remove_card ([(Hearts, Ace)], (Diamonds, Queen), IllegalMove) handle IllegalMove => []

fun all_same_color(cs) = 
        case cs of 
            c1::c2::cs' => card_color(c1) = card_color(c2) andalso all_same_color(c2::cs')
        | _ => true


val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test8_1 = all_same_color [(Hearts, Ace), (Spades, Num 5)] = false
val test8_2 = all_same_color [(Hearts, Ace), (Diamonds, Queen), (Hearts, Num 7)] = true
val test8_3 = all_same_color [(Hearts, Ace)] = true

fun sum_cards(cs) =
        let
            fun helper(cs, rsf) = 
                    case cs of 
                        [] => rsf
                    | c::cs' => helper(cs', rsf + card_value(c))
        in
            helper(cs, 0)
        end

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4

fun score (cs, goal) =
        let
            val sum = sum_cards cs
            val preliminary_score = if sum > goal then 3 * (sum - goal) else goal - sum
            val final_score = if all_same_color cs then preliminary_score div 2 else preliminary_score
        in
            final_score
        end


val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4


fun member (c, cs) = 
        case cs of 
            [] => false 
        | card :: cs' => if card = c 
            then true 
            else member(c, cs')


fun officiate (cards, moves, goal) =
        let
            fun helper(cards, moves, goal, held_cards) =
                    case moves of
                        [] => score(held_cards, goal)
                    | Draw :: moves' => (
                            case cards of
                                [] => score(held_cards, goal)
                            | c :: cards' =>
                                let
                                    val held_cards' = c :: held_cards
                                    val sum = sum_cards(held_cards')
                                in
                                    if sum > goal
                                    then score(held_cards', goal)
                                    else helper(cards', moves', goal, held_cards')
                                end
                        )
                    | Discard c :: moves' =>
                        if member(c, held_cards)
                        then helper(cards, moves', goal, remove_card(held_cards, c, IllegalMove))
                        else raise IllegalMove
        in
            helper(cards, moves, goal, [])
        end

(* val test10_1 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test10_2 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
        [Draw,Draw,Draw,Draw,Draw], 42) = 3

val test10_3 = 
    ((officiate([(Clubs,Jack),(Spades,Num(8))],
                [Draw,Discard(Hearts,Jack)],
                42);
            false) 
        handle IllegalMove => true) *)

fun sum_cards_challenge(cs, goal) =        
        let fun card_value(card, sum_so_far) =
                    case card of 
                        (_, Num n) => n
                    | (_, Ace) => if goal - sum_so_far >= 11 then 11 else 1
                    | _ => 10
        
            fun helper(cs, sum_so_far) = 
                    case cs of 
                        [] => sum_so_far
                    | c::cs' => helper(cs', sum_so_far + card_value(c, sum_so_far))
        in
            helper(cs, 0)
        end 
        
fun score_challenge (cs, goal) =
        let 
            val sum = sum_cards_challenge(cs, goal) 
            val preliminary_score = if sum > goal then 3 * (sum - goal) else goal - sum
            val final_score = if all_same_color cs then preliminary_score div 2 else preliminary_score
        in
            final_score
        end


val test11_1 = score_challenge([(Hearts, Num 2), (Clubs, Num 4)], 21) = 15
val test11_2 = score_challenge([(Hearts, Ace), (Clubs, Num 4)], 21) = 6
val test11_3 = score_challenge([(Hearts, Ace), (Clubs, Ace)], 21) = 9
val test11_4 = score_challenge([(Hearts, Num 10), (Clubs, Num 10), (Spades, Num 10)], 21) = 27


fun officiate_challange (cards, moves, goal) =
        let
            fun helper(cards, moves, goal, held_cards) =
                    case moves of
                        [] => score_challenge(held_cards, goal)
                    | Draw :: moves' => (
                            case cards of
                                [] => score_challenge(held_cards, goal)
                            | c :: cards' =>
                                let
                                    val held_cards' = c :: held_cards
                                    val sum = sum_cards_challenge(held_cards', goal)
                                in
                                    if sum > goal
                                    then score_challenge(held_cards', goal)
                                    else helper(cards', moves', goal, held_cards')
                                end
                        )
                    | Discard c :: moves' =>
                        if member(c, held_cards)
                        then helper(cards, moves', goal, remove_card(held_cards, c, IllegalMove))
                        else raise IllegalMove
        in
            helper(cards, moves, goal, [])
        end



