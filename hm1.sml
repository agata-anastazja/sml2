
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


fun number_before_reaching_sum (sum: int, input: int list) = 
     let  
     fun find_indx(sum: int, input: int list, indx: int) =
      if (sum - (hd input) <= 0)
          then indx
          else find_indx ((sum - (hd input)), (tl input), (indx + 1))
     in find_indx(sum, input, 0)
     end

fun what_month (day: int) = 
     let val month_lenghts = [31, 28, 31, 30, 31, 30, 31, 31,30,31,30,31]
     in number_before_reaching_sum(day, month_lenghts) + 1
     end

(* Write a function month_range that takes two days of the year day1 and day2 and returns an int list
[m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month
of day day2. Note the result will have length day2 - day1 + 1 or length 0 if day1>day2. *)

fun month_range (day1: int, day2: int) = 
     if day1 > day2
     then []
     else what_month(day1) :: month_range((day1+1), day2)

           
