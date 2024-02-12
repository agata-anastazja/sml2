(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

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



(**** you can put all your code here ****)

fun only_capitals (s_list) = 
 List.filter (fn(x) => Char.isUpper (String.sub (x, 0))) s_list

 fun longest_string1(s_list) = 
    List.foldl (fn (x, y)=> if String.size x > String.size y then x else y) "" s_list

fun longest_string2 (s_list) =
    List.foldl (fn (x, y)=> if String.size x >= String.size y then x else y) "" s_list


val  longest_string_helper = fn f => fn s_list => 
    List.foldl (fn (x, y)=> f(x,y)) "" s_list

val longest_string3 = longest_string_helper (fn (x, y)=> if String.size x > String.size y then x else y)  
val longest_string4 = longest_string_helper (fn (x, y)=> if String.size x >= String.size y then x else y)  

val longest_capitalized = longest_string3 o only_capitals

val rev_string = String.implode o rev o String.explode

fun first_answer f = fn list =>
    case list of 
    [] => raise NoAnswer
    | x::xs' => (case f x of
                 SOME m => m
                 | NONE => first_answer f xs')

fun all_answers f = fn list => 
    let fun some_answers (f, list, acc) = 
            case list of
            [] => NONE
            | x::[] => (case f x of
                            NONE => NONE 
                            | SOME m => SOME (m @ acc))
            | x::xs' => (case f x of
                            NONE => NONE 
                            | SOME m => some_answers (f, xs', (m @ acc)))
    in some_answers(f, list, [])
    end

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

val count_wildcards = g (fn() => 1) (fn(_)=> 0)

val count_wild_and_variable_lengths = g (fn() => 1) (fn(x) => (String.size x))

val count_some_var = fn (str, p) => g (fn()=> 0) (fn(x) => (if x = str then 1 else 0)) p


val check_pat = fn p =>
    let fun get_values (pattern, acc) = 
            case pattern of
                Variable x          => x::acc
                | TupleP ps         => List.foldl (fn (p, i) => (get_values(p, i)  )) [] ps
                | ConstructorP(_,p) => get_values (p, acc) @ acc 
                | _                 => acc
        fun repetitions_exsist (list) = 
            case list of 
            [] => false
            | x::xs' => (List.exists (fn(y) => y = x) xs') orelse repetitions_exsist(xs')
    in  
        not (repetitions_exsist (get_values (p, [])))
    end 

(*  Write a function match that takes a valu * pattern and returns a (string * valu) list option,
namely NONE if the pattern does not match and 
SOME lst where lst is the list of bindings if it does.
Note that if the value matches but the pattern has no patterns of the form Variable s, then the result
is SOME []. 

Hints: Sample solution has one case expression with 7 branches. 
The branch for tuples
uses all_answers and ListPair.zip. Sample solution is 13 lines.
 Remember to look above for the rules for what patterns match what values, and what bindings they produce. These are hints: We are
not requiring all_answers and ListPair.zip here, but they make it easier. *)
(* match (Const(1), ConstP 1) *)
fun match (valu, pattern) = 
    case (valu, pattern) of
        (_, Variable x) =>  SOME [(x, valu)] 
        | (Unit, UnitP)  => SOME []
        | (_, Wildcard) => SOME []
        | (Const x, ConstP y) => if x = y then SOME [] else NONE
        | (Tuple ys, TupleP ps) => if List.length(ys) = List.length(ps)
                                        then let fun check_tuple (list_of_pairs) = 
                                                    all_answers match list_of_pairs
                                                in check_tuple(ListPair.zip(ys, ps))
                                                end
                                        else NONE
        | (Constructor (s1, v), ConstructorP (s2, p)) => NONE
        | _ => NONE
(* (ListPair.zip(ys, ps)) *)
(* match (Const(1), ConstP 1) *)

(* match (Tuple [Const(1), Const(1)], ConstP 1)
match (Tuple [Const(1), Const(1)], TupleP [ConstP 1, ConstP 1])

match (Tuple [Const(1), Const(1), Const(3)], TupleP [ConstP 1, ConstP 1, Variable "z"]) *)