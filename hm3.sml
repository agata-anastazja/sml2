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


(**** you can put all your code here 
Write a function longest_string2 that is exactly like longest_string1 except in the case of ties
it returns the string closest to the end of the list. Your solution should be almost an exact copy of
longest_string1. Still use foldl and String.size.
****)

fun only_capitals (s_list) = 
 List.filter (fn(x) => Char.isUpper (String.sub (x, 0))) s_list

(* List.foldl (fn (x,y) => x+y) 0 [3,4,5], *)
 fun longest_string1(s_list) = 
    List.foldl (fn (x, y)=> if String.size x > String.size y then x else y) "" s_list

fun longest_string2 (s_list) =
    List.foldl (fn (x, y)=> if String.size x >= String.size y then x else y) "" s_list