(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2


fun all_except_option(s1: string, slist: string list) = 
   case slist of
   [] => NONE
   | x::slist' => if same_string(x, s1)
                    then SOME slist'
                    else case all_except_option(s1, slist') of
                        NONE => NONE
                        | SOME ls => SOME (x::ls)


fun get_substitutions1 (l1, s1) =
    case l1 of
    [] => []
    | x::xs' => case all_except_option(s1,x ) of
                    NONE => get_substitutions1(xs', s1)
                    | SOME ls => ls @ get_substitutions1(xs', s1)

fun get_substitutions2 (l1, s1) =
    let fun get_substitutions_rec (l1, s1, acc) = 
        case l1 of
        [] => [] @ acc
        | x::xs' => case all_except_option(s1,x ) of
                    NONE => get_substitutions_rec(xs', s1, acc)
                    | SOME ls => get_substitutions_rec(xs', s1, (acc @ ls))
    in get_substitutions_rec(l1, s1, [])
    end

fun similar_names (l1, {first=x,middle=y,last=z}) = 
    let val substitutes = get_substitutions2(l1, x) 
        fun all_versions_of_name (versions, {first=x,middle=y,last=z} ) = 
            case versions of
            [] => []
            | a::xs' => {first=a,middle=y,last=z} :: all_versions_of_name(xs', {first=x,middle=y,last=z})
    in all_versions_of_name(x::substitutes, {first=x,middle=y,last=z})
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank


datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove


fun card_color (x,y) = 
    case x of
    Spades =>  Black
    | Clubs => Black
    | Diamonds => Red
    | Hearts => Red

fun card_value(x,y) = 
    case y of
    Num a => a
    | Ace => 11
    | _ => 10



fun remove_card (cs, c, e) = 
    case cs of 
        [] => raise e
        | x::xs' => if x = c 
                    then xs'
                    else (case remove_card(xs', c, e) of 
                            ls => (x::ls))

fun all_same_color(cards) = 
    case cards of
    [] => true
    | _ :: [] => true 
    | head :: (neck :: rest) => (card_color head = card_color neck andalso all_same_color (neck :: rest))

fun sum_cards (cards) = 
    let fun get_values(cards, acc) = 
            case cards of 
            [] => 0 + acc
            | x::[] => card_value(x) + acc
            | x::xs' => get_values(xs', (card_value(x) + acc))
            
    in get_values(cards, 0)
    end


fun score (held_cards, goal) = 
    let val sum = sum_cards(held_cards)
        val same_color = all_same_color(held_cards)
        val potential_score = (sum - goal) * 3
        val potential_score_2 = goal - sum
    in case ((sum > goal), same_color) of
        (true, true) => potential_score div 2
        | (true, _) => potential_score
        | (_, true) => potential_score_2 div 2
        | _ => potential_score_2
    end


fun officiate (card_list, move_list, goal) = 
    let fun make_moves(card_list, moves, held_cards) = 
            case moves of 
                [] => []
                | x::xs' => (case (x, card_list) of
                                (Draw, []) => held_cards
                                | (Draw, y::ys') => if ((score ( y::held_cards, goal)) > goal)
                                                    then (y::held_cards)
                                                    else make_moves(ys', xs', (y::held_cards))
                                | (Discard card, y) => make_moves(y, xs', (remove_card(held_cards, card, IllegalMove))))
        val held_cards = make_moves(card_list, move_list, [])
    in score(held_cards, goal)
    end