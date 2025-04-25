(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(name, names_list) = 
   let
     fun aux(strings, acc) = 
         case strings of 
            [] => NONE |
            s'::strings' => if same_string(name, s') then SOME(acc@strings') else aux(strings', s'::acc)
   in
     aux(names_list, [])
   end

fun get_substitutions1(substitutions, s) = 
   case substitutions of
      [] => [] |
      x::xs => let val curr = all_except_option(s, x) in
         case curr of
            NONE => get_substitutions1(xs, s) | SOME y => y @ get_substitutions1(xs, s)
         end

fun get_substitutions2(substitutions, s) = 
   let
      fun aux(lists, acc) = 
         case lists of 
            [] => acc |
            x::xs => 
               let 
                  val curr = all_except_option(s, x) 
               in
                  case curr of
                     NONE => aux(xs, acc) |
                     SOME y => aux(xs, acc@y)
               end
   in
      aux(substitutions, [])
   end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
