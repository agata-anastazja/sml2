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