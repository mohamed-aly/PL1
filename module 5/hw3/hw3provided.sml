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


(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

val only_capitals = List.filter (fn s => Char.isUpper(String.sub(s, 0)))

val longest_string1 = List.foldl (fn (x, y) => if String.size(x) > String.size(y) then x else y) "" 

val longest_string2 = List.foldl (fn (x, y) => if String.size(x) >= String.size(y) then x else y) ""

fun longest_string_helper cmp = List.foldl (fn (x, y) => if cmp (String.size x, String.size y) then x else y) ""

val longest_string3 = longest_string_helper (fn (x, y) => if x > y then true else false)

val longest_string4 = longest_string_helper (fn (x, y) => if x >= y then true else false)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f l = 
	case l of 
		[] => raise NoAnswer |
		x::xs => let val answer = f(x) in
		case answer of NONE => first_answer f xs | SOME(v) => v end

fun all_answers f l =
	let
		fun aux(l', acc) = 
			case l' of 
				[] => SOME acc |
				x::xs => let val answer = f(x) in
					case answer of NONE => NONE | SOME v => aux(xs, acc@v) end

	in
		aux(l, [])
	end


fun g f1 f2 p = 
	let
		val r = g f1 f2
	in
		case p of
			Wildcard => f1 () |
			Variable x => f2 x |
		 	TupleP ps => List.foldl (fn (p, acc) => (r p) + acc) 0 ps |
			ConstructorP (_, p) => r p |
			_ => 0
	end


val count_wildcards = g (fn () => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn () => 1) (fn x => String.size x)

fun count_some_var (s, p) = g (fn () => 0) (fn x => if x = s then 1 else 0) p

fun check_pat p = 
	let
		fun extract_vars p =
			case p of
				Variable x => [x] |
				TupleP ps => List.foldl (fn (p, acc) => extract_vars(p) @ acc) [] ps |
				ConstructorP (_, p) => extract_vars p |
				_ => []

		fun repeats sl =
			case sl of
				[] => true |
				x::xs => List.exists (fn a => if a = x then true else repeats xs) sl 
	in
		repeats(extract_vars(p))
	end

fun match(v, p) = 
	case (v, p) of
		(_, Wildcard) => SOME[] |
		(_, Variable s) => SOME[(s, v)] |
		(Unit, UnitP) => SOME[] |
		(Const x, ConstP n) => if x = n then SOME[] else NONE |
		(Tuple vl, TupleP pl) => 
			((let 
				val compined = ListPair.zipEq(vl, pl) 
			in 
				all_answers match compined
			end) handle e => NONE) |
		(Constructor (s2, v'), ConstructorP (s1, p')) => if s1=s2 then match(v', p') else NONE |
		_ => NONE




	


