(*fun is_older(a: int*int*int, b: int*int*int)=
    if (#1 a > #1 b)
    then a
    else if (#1 b > #1 a)
    then b
    else
	if(#2 a > #2 b)
	then a
	else if(#2 b > #2 a)
	then b
	else
	    if(#3 a > #3 b)
	    then a
	    else b*)

fun is_older(a: int*int*int, b: int*int*int)=
    if (#1 a > #1 b)
    then false
    else if (#1 b > #1 a)
    then true
    else
	if(#2 a > #2 b)
	then false
	else if(#2 b > #2 a)
	then true
	else
	    if(#3 a < #3 b)
	    then true
	    else false



fun number_in_month(a : (int*int*int) list, b : int) =
    if null a
    then 0
    else if( #2 (hd(a)) = b)
	       then 1 +  number_in_month(tl (a), b)
         else number_in_month(tl (a), b)
		   
fun number_in_months (a: (int * int * int ) list, b: int list) =
    if null b
    then 0
    else number_in_month(a, hd(b)) + number_in_months(a, tl(b))

fun append (xs : (int*int*int) list, ys : (int*int*int) list) = (* part of the course logo :) *)
    if null xs
    then ys
    else hd(xs) :: append(tl(xs), ys)

fun dates_in_month(a : (int * int * int) list, b: int)=
    if null a
    then []
    else if(#2 (hd(a)) = b)
    then hd(a) :: dates_in_month(tl (a), b)
    else dates_in_month(tl(a),b)


		       
fun dates_in_months(a: (int * int * int ) list, b: int list)=
    if null b
    then []
    else append(dates_in_month(a, hd(b)), dates_in_months(a, tl(b)))



fun get_nth (a: string list, b: int)=
    if null a
    then ""
    else if (b = 1)
    then hd(a)
    else get_nth(tl(a), b-1)



fun date_to_string(a: (int*int*int))=
    let val month = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth(month, #2 a) ^" " ^ Int.toString(#3 a)^ ", " ^ Int.toString(#1 a)
    end
	


(*fun number_before_reaching_sum(sum : int , sum_list : int list) =
    if sum <= hd(tl (sum_list))
    then hd(sum_list)
    else number_before_reaching_sum(sum - hd(sum_list), tl(sum_list))*)



			

fun number_before_reaching_sum(sum : int , sum_list : int list) =
    if null sum_list
    then 0
    else if sum <= hd ( sum_list)
    then 0
    else
	if sum- hd(sum_list) <= hd(tl(sum_list))
	then 1
	else 1 + number_before_reaching_sum(sum - hd(sum_list), tl(sum_list))

fun  what_month (a : int)=
     let
	 val a_list = [31,28,31,30,31,30,31,31,30,31,30,31]
     in
	 1+number_before_reaching_sum(a, a_list)
     end

	 

fun month_range(a : int, b : int ) =
    if (b < a)
    then []
    else what_month(a):: month_range(a+1,b)


fun oldest (a : (int*int*int) list )=
    if null a
    then
	NONE
    else let
	fun max_oldest(a : (int * int * int) list) =
	    if null (tl a)
	    then hd a
	    else let val tl_ans = max_oldest(tl a)
		 in
		     if is_older(hd (a), tl_ans)
		     then hd (a)
		     else tl_ans
		 end
    in
	SOME(max_oldest a)
    end


(*fun number_in_months()*)


fun get_nth_num (a: int list, b: int)=
    if null a
    then 0
    else if (b = 1)
    then hd(a)
    else get_nth_num(tl(a), b-1)



fun reasonable_date(a : int * int * int ) =
    let 
	val a_list = [31,28,31,30,31,30,31,31,30,31,30,31]
	val b_list =  [31,29,31,30,31,30,31,31,30,31,30,31]
    in
    if #1 a <= 0
    then false
    else if #2 a <= 0 orelse #2 a > 12
    then false
    else if #3 a <= 0 orelse #3 a > 31
    then false
    else
	if ((#1 a mod 4 = 0 andalso #1 a mod 100 <> 0) orelse (#1 a mod 400 = 0))
        then if (#3 a <= 0 orelse #3 a > get_nth_num(b_list, #2 a))
		  then false
		  else true	   
    else if(#3 a <= 0 orelse #3 a > get_nth_num(a_list, #2 a))
	 then false
	 else true
	 
    end

fun in_list(a : int, b: int list)=
    if null b
    then false
    else if ( a = hd(b))
    then true
    else in_list(a , tl(b))

fun number_in_months_challenge (a: (int * int * int ) list, b: int list) =
    if null b
    then 0
    else if (in_list(hd(b), tl(b)) = true)
	then 0 + number_in_months_challenge(a, tl(b))
	else number_in_month(a, hd(b)) + number_in_months_challenge(a, tl(b))
					
fun dates_in_months_challenge (a : (int*int*int) list, b: int list)=
    if null b
    then []
    else if (in_list(hd(b), tl(b)) = true)
	then  dates_in_months_challenge(a, tl(b))
	else append(dates_in_month(a, hd(b)), dates_in_months_challenge(a, tl(b)))
					
