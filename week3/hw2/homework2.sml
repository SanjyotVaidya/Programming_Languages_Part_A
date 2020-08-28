fun same_string(s1 : string, s2 : string) =
    s1 = s2

    

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove



fun all_except_option(s1,s2) =
    let
	fun all_except_option_helper(s1, s2) =
    case s2 of
	[] => false
      | s::s2' => same_string(s1,s) orelse all_except_option_helper(s1,s2')
    in
	
	case all_except_option_helper(s1,s2) of
		 false  => []
	       | _ =>
		 let
		     fun all_except_op(s1,s2) =
		     case s2 of
			    [] => []
			       |  s ::s2' => if  same_string(s1,s)
		     then  all_except_op(s1,s2')
			else s :: all_except_op(s1,s2')
		 in
		     all_except_op(s1,s2)
		 end
    end
	



fun get_substitutions1(s1,s2)=
    case s1 of
	[] => []
      | s :: s1'  => case all_except_option(s2, s) of
					  [] => get_substitutions1(s1', s2)
			| s2' => s2' @ get_substitutions1(s1',s2)





							      
fun get_substitutions2(s1,s2) =
    let fun helper(s1,s2,acc) =
	    case s1 of
		[] => acc
	      | s :: s1' => case all_except_option(s2,s) of
				[] =>  helper(s1', s2, acc)
			      |  s2' => helper(s1',s2, s2' @ acc)
    in
	helper(s1,s2,[])
    end


	

fun similar_names (string_list, {first =x , middle= y , last=z})=
 let fun help_similar_names(str_list, {first = x, middle = y, last = z}, acc)=
	      case str_list of
		       [] => acc
		 | str1 :: str_list' => help_similar_names(str_list', {first = x,middle = y,last = z},  {first = str1, middle =y, last =z} ::acc )
 in
     let val first_names = get_substitutions2(string_list, x)
     in
	  {first = x, middle = y, last = z} :: help_similar_names(first_names, {first = x, middle = y, last = z}, [])
     end
  end

fun card_color(s, r) =
    case s of
	Clubs => Black
      | Spades => Black
      | Diamonds => Red
      | Hearts => Red 


fun card_value (s,r)=
    case r of
	Num  y => y
     |  Ace => 11
     | _ => 10



fun remove_card (cs  , c, e)=
    let
	fun find_card (cs:card list, c:card)=
	        case cs of
		    [] => false
		  | c2::cs' => c = c2 orelse find_card(cs',c) 
    in
	case find_card(cs, c) of
	    false => raise e
	   |_ =>  case cs of
			    [] => []
			       |  s ::cs' => if  c = s
		     then  cs'
		     else s :: remove_card(cs',c, e)
    end
	
		      

fun all_same_color (cs) =
    case cs of
       [] => true
      | c :: c2 :: cs' =>  card_color(c) = card_color(c2) andalso (all_same_color(c2 :: cs'))
      | _ => true 
    

fun sum_cards(cs)=
    let fun sum_cards_tail (cs, acc) =
	case cs of
	   [] => acc
	 | c :: cs' => sum_cards_tail(cs', card_value(c)+acc)
    in
	sum_cards_tail(cs, 0)
    end



fun score (cs, goal)=
    let
	val a = sum_cards(cs)
    in
	if a > goal then
	    if all_same_color(cs) then
		(3 * (a - goal)) div 2
	    else
		(3 * (a - goal))
	else
	    if all_same_color(cs) then
		(goal - a) div 2
	    else
		(goal - a)
    end

			
fun officiate(cs, ms, goal) =
    let fun find_held_list (cs, ms, acc) =
	    case ms of
		[] => acc
	      | Draw :: ms' =>(case cs of
			    [] =>acc
				| c :: cs' => if (sum_cards(acc) > goal) then acc
					      else find_held_list(cs', ms' ,c :: acc))
	      |Discard y::ms'  =>if (sum_cards(acc) > goal) then acc
					      else find_held_list(cs, ms',remove_card(acc,y,IllegalMove))
    in
	let val held_list = find_held_list(cs,ms,[])
	in
		score(held_list, goal)
	end
	(*find_held_list(cs,ms,[])*)
	    
    end


