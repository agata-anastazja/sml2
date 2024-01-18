
fun is_older (date1 : int*int*int, date2 : int*int*int) = 
 let val x =[((#1 date1), (#1 date2)), ((#2 date1), (#2 date2)), ((#3 date1), (#3 date2))]
     fun compare (d1:  (int * int) list) =
          if (null d1 orelse (#1 (hd d1)) > (#2 (hd d1)))
          then false
          else if (#1 (hd d1)) < (#2 (hd d1))
               then true
               else compare (tl d1)
 in compare x
 end

 fun number_in_month (input : (int * int * int) list, month: int) =
     if null input
      then     0
     else if (#2 (hd input)) = month
          then 1 + number_in_month (tl input, month)
          else 0 + number_in_month (tl input, month)

fun number_in_months (input : (int * int * int) list, months: int list) =
     if null months
          then 0
          else number_in_month(input, (hd months)) + number_in_months(input, (tl months))

fun dates_in_month (input : (int * int * int) list, month: int) =
     if null input
      then     []
     else if (#2 (hd input)) = month
          then  (hd input) :: dates_in_month ((tl input), month)
          else  dates_in_month ((tl input), month)

fun dates_in_months (input : (int * int * int) list, months: int list) =
     if null months
          then []
          else dates_in_month(input, (hd months)) @ dates_in_months(input, (tl months))

fun get_nth (input : string list , indx : int) =
     if indx = 1
         then hd input
         else get_nth((tl input), (indx - 1))

fun date_to_string (date : int*int*int) =
     let val months = ["January", "February", "March", "April",
"May", "June", "July", "August", "September", "October", "November", "December"]
     in get_nth(months, (#2 date)) ^ " " ^ (Int.toString (#3 date)) ^ ", " ^ (Int.toString (#1 date))
     end

(* Write a function number_before_reaching_sum that takes an int called sum, which you can assume
is positive, and an int list, which you can assume contains all positive numbers, and returns an int.
You should return an int n such that the first n elements of the list add to less than sum, but the first
n + 1 elements of the list add to sum or more. Assume the entire list sums to more than the passed in
value; it is okay for an exception to occur if this is not the case. *)

fun number_before_reaching_sum (sum: int, input: int list) = 
     let  
     fun find_indx(sum: int, input: int list, indx: int) =
      if (sum - (hd input) <= 0)
          then indx
          else find_indx ((sum - (hd input)), (tl input), (indx + 1))
     in find_indx(sum, input, 1)
     end