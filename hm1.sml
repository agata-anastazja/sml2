
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