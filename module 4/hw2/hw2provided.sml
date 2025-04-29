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

fun similar_names(substitutions, {first,last,middle}) = 
   let
      val first_name_substitution = get_substitutions2(substitutions, first)
      fun aux(names) = 
         case names of
            [] => [] |
            n::ns => { first = n, middle = middle, last = last }::aux(ns) 
   in
      {first=first, last=last, middle=middle}::aux(first_name_substitution)
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

fun card_color(c) = 
   case c of
      (Spades, _) => Black |
      (Clubs, _) => Black |
      _ => Red


fun card_value(c) = 
   case c of
      (_, Num n) => n |
      (_, Ace) => 11 |
      _ => 10

fun remove_card(cs, c, e) =
   case cs of
      [] => raise e |
      c'::cs' => if c = c' then cs' else c'::remove_card(cs', c, e)

fun all_same_color(cs) = 
   case cs of
      [] => true |
      x::[] => true |
      x::y::rest => card_color(x) = card_color(y) andalso all_same_color(rest)

fun sum_cards(cs) =
   let
      fun aux(updated_cs, acc) = 
         case updated_cs of
            [] => acc |
            x::xs => aux(xs, card_value(x) + acc)
   in
      aux(cs, 0)
   end

fun score(cs, goal) = 
   let
      val sum = sum_cards(cs)
      val preliminary_score = if sum > goal then (sum - goal) * 3 else goal - sum
   in
      if all_same_color(cs) then preliminary_score div 2 else preliminary_score
   end

fun officiate(cards_list, moves_list, goal) = 
   let
      fun aux(updated_cards_list, held_cards, moves) = 
         case moves of
            [] => score(held_cards, goal) |
            m::ms => 
               case m of
                  Discard c => aux(updated_cards_list, remove_card(held_cards, c, IllegalMove), ms) |
                  Draw => 
                     case updated_cards_list of
                        [] => score(held_cards, goal) |
                        c'::cs' => 
                           let
                              val updated_held_cards = c'::held_cards
                              val score = score(updated_held_cards, goal)
                           in
                              if score > goal then score else aux(cs', updated_held_cards, ms) 
                           end
   in
      aux(cards_list, [], moves_list)
   end
