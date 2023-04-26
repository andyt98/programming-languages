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


fun card_value(card) = 
        case card of 
            (_, Num n) => n
        | (_, Ace) => 11
        | _ => 10

fun remove_card(cs, c, e) =
        case cs of
            [] => raise e
        | card::cs' => if card = c
            then cs'
            else card::remove_card(cs', c, e)

fun all_same_color(cs) = 
        case cs of 
            c1::c2::cs' => card_color(c1) = card_color(c2) andalso all_same_color(c2::cs')
        | _ => true

fun sum_cards(cs) =
        let
            fun helper(cs, rsf) = 
                    case cs of 
                        [] => rsf
                    | c::cs' => helper(cs', rsf + card_value(c))
        in
            helper(cs, 0)
        end

fun score (cs, goal) =
        let
            val sum = sum_cards cs
            val preliminary_score = if sum > goal then 3 * (sum - goal) else goal - sum
            val final_score = if all_same_color cs then preliminary_score div 2 else preliminary_score
        in
            final_score
        end

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



