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


datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string


fun func v = Char.isUpper(String.sub (v, 0))
fun only_capitals xs = List.filter func xs




fun longest_string1 xs =
      List.foldl (fn (x, y) => if (String.size(x) > String.size(y)) then x else y) ""  xs 


fun longest_string2 xs =
    List.foldl (fn (x, y) => if (String.size(x) >= String.size(y)) then x else y) ""  xs 


fun longest_string_helper f = 
    List.foldl (fn (s,sofar) => if f(String.size s,String.size sofar)
				                        then s
				                        else sofar) 
	                              ""
val longest_string3 = longest_string_helper (fn (x,y) => x > y) 
val longest_string4 = longest_string_helper (fn (x,y) => x >= y)




fun  longest_capitalized xs =
     longest_string1 (only_capitals xs)

fun rev_string s = (String.implode o rev o String.explode) s

		   
fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | x :: xs' => (case f (x) of
		    NONE => first_answer f xs'
		      |  SOME v => v)

fun all_answers try xs = 
  let fun acc(SOME(a), SOME(b)) = SOME(b@a)
        | acc(_,_)              = NONE
  in
    List.foldl (fn(x,y) => acc(try(x), y)) (SOME[]) xs
  end


fun count_wildcards p =
    g (fn () => 1) (fn (x) => 0) p


fun  count_wild_and_variable_lengths p =
     g (fn () => 1) (fn (x) => String.size(x)) p

fun count_some_var ( str, p )=
    g (fn () => 0) (fn(x) => if x = str then 1 else 0) p




fun check_pat p =
    let
	fun return_string_list p =
	    let		
		val r = return_string_list
	    in
		case p of
		    Wildcard          => []
		  | Variable x        => [x]
		  | TupleP ps         => List.foldl (fn (p,acc) => (r p) @ acc) ([] ) ps
		  | ConstructorP(_,p) => r p
		  | _                 => []
	    end      
	fun take_list_return_bool (xs: string list) =
	    case xs of
		[] => false
	      | x :: xs'  => List.exists (fn (y) => x = y) xs' orelse take_list_return_bool (xs')

    in
	not ( take_list_return_bool (return_string_list p))
    end
	
(* problem 11 *)
fun match (v, p)=
    case p of
	Wildcard => SOME []
      | Variable s  => SOME [(s,v)]
      | UnitP  => (case v of
		       Unit => SOME []
		    | _ => NONE )
      | ConstP i => (case v of
			 Const j => (if i = j then SOME []
				     else NONE)
		      | _ => NONE )  
      | ConstructorP (s, p1) => (case v of
				     Constructor (s2,v1) => if s = s2 then match(v1,p1)
							    else NONE
				  | _ => NONE )
      | TupleP [] => (case v of
			  Tuple [] => SOME []
		       | _ => NONE)
      | TupleP (p1 :: ps) => (case v of
			      Tuple (v1 :: vs) => (case match(v1,p1) of
							 NONE => NONE
						       | SOME y => (*SOME y *)(case match (Tuple (vs), TupleP (ps)) of
									NONE => NONE
								     | SOME a => SOME (y @ a) ))
			       |  _ => NONE )


			 
 


fun first_match v ps =
	SOME (first_answer (fn p => match(v, p) ) ps)
	handle NoAnswer => NONE		      
