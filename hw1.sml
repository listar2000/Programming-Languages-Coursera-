(*function 1*)
fun is_older(date1: (int*int*int), date2: (int*int*int)) =
    if (#1 date1 > #1 date2)
    then false
    else if (#1 date1 < #1 date2)
    then true
    else if (#2 date1 > #2 date2)
    then false
    else if (#2 date1 < #2 date2)
    then true
    else if (#3 date1 > #3 date2)
    then false
    else if (#3 date1 < #3 date2)
    then true
    else false
	     
(*function 2*)
fun number_in_month(date_list: (int*int*int)list, month: int) =
    if null date_list
    then 0
    else if (#2(hd date_list)) = month
    then 1 + number_in_month((tl date_list),month)
    else number_in_month((tl date_list),month)

(*function 3*)
fun number_in_months(date_list: (int*int*int)list, months: int list) =
    if null date_list orelse null months
    then 0
    else number_in_month(date_list, hd months) + number_in_months(date_list, tl months)
	    
(*function 4*)
fun dates_in_month(date_list: (int*int*int)list, month: int) =
    if null date_list
    then []
    else if (#2(hd date_list)) = month
    then ((hd date_list))::dates_in_month((tl date_list),month)
    else dates_in_month((tl date_list),month)
	
(*fun dates_in_months(date_list: (int*int*int)list, months: int list) =
    if null months orelse null date_list
    then []
    else dates_in_month(date_list, hd months) @ dates_in_months(date_list, tl months)*)
		       
(*function 5*)							      
fun dates_in_months(date_list: (int*int*int)list, months: int list) =
    if null months orelse null date_list
    then []
    else let
	fun append(xs: (int*int*int) list, ys: (int*int*int) list) =
	    if null xs
	    then ys
	    else	
	    (hd xs)::append(tl xs, ys)
    in append(dates_in_month(date_list, hd months), dates_in_months(date_list, tl months))
    end

(*function 6*)
fun get_nth(strings: string list, n: int) =
    if n = 1
    then hd strings
    else get_nth(tl strings, n-1)

(*function 7*)
fun date_to_string(date: int*int*int) =
    let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in get_nth(months,#2 date)^" "^Int.toString(#3 date)^", "^Int.toString(#1 date)
    end

(*function 8*)
fun number_before_reaching_sum(n: int, number: int list) =
    let val sum = hd number 
	fun helper(number: int list, count: int, sum: int) =
	    if sum >= n
	    then count-1
	    else let val sum = sum + (hd (tl number))
		 in helper(tl number, count+1, sum)
		 end
    in helper(number, 1, sum)
    end

(*function 9*)
fun what_month(day: int) =
    let val list = [31,28,31,30,31,30,31,31,30,31,30,31]
    in number_before_reaching_sum(day, list)+1
    end

(*function 10*)
fun month_range(day1: int, day2: int) =
    if day2<day1
    then []
    else let fun createList(from: int, to: int) =
		 if from > to
		 then []
		 else from::createList(from+1, to)
	     val list = createList(day1, day2)		  
	     fun createMonth(list: int list) =
		 if null list
		 then []
		 else what_month(hd list)::createMonth(tl list)
	 in createMonth(list)
	 end
	     
(*function 11*)
fun oldest(list: (int*int*int)list) =
    if null list
    then NONE
    else if null (tl list)
    then SOME (hd list)
    else let fun returnSmaller(day1:(int*int*int), day2: (int*int*int)) =
	         if is_older(day1, day2) = true
		 then day1
		 else day2
	 in SOME (returnSmaller(hd list, valOf(oldest(tl list))))
	 end
	     
		     
										 
	     
	     
		     
			 
	     
	
	
    
	
	
	
	     
 
		       
