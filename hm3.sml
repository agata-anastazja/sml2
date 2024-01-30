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

(* List.foldl (fn (x,y) => x+y) 0 [3,4,5], *)
 fun longest_string1(s_list) = 
    List.foldl (fn (x, y)=> if String.size x > String.size y then x else y) "" s_list

fun longest_string2 (s_list) =
    List.foldl (fn (x, y)=> if String.size x >= String.size y then x else y) "" s_list

(*  Write functions longest_string_helper, longest_string3, and longest_string4 such that:
• longest_string3 has the same behavior as longest_string1 and longest_string4 has the
same behavior as longest_string2.
• longest_string_helper has type (int * int -> bool) -> string list -> string
(notice the currying). This function will look a lot like longest_string1 and longest_string2
but is more general because it takes a function as an argument.
• If longest_string_helper is passed a function that behaves like > (so it returns true exactly
when its first argument is stricly greater than its second), then the function returned has the same
behavior as longest_string1.
• longest_string3 and longest_string4 are defined with val-bindings and partial applications
of longest_string_helper. *)

val  longest_string_helper = fn f => fn s_list => 
    List.foldl (fn (x, y)=> f(x,y)) "" s_list

val longest_string3 = longest_string_helper (fn (x, y)=> if String.size x > String.size y then x else y)  
val longest_string4 = longest_string_helper (fn (x, y)=> if String.size x >= String.size y then x else y)  
