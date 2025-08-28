(* 1 *)
fun compose_opt f g x = 
    case g x of 
        NONE => NONE |
        SOME X => f x

fun safeSqrt n = if n < 0 then NONE else SOME(Math.sqrt( real n))

fun halfIfEven n = if n mod 2 = 0 then SOME(n div 2) else NONE

val h = compose_opt halfIfEven safeSqrt

val s = h 16

(* 2 *)
fun do_until f p x = 
    if p x
    then do_until f p (f x)
    else x

val s = do_until (fn x => x div 2) (fn x => x mod 2 <> 1) 100

(* 3 *)
fun factorial n = 
    let
      val (_, result) = do_until (fn (i, acc) => (i-1, acc * i)) (fn (i, _) => i <> 1) (n, 1)
    in
      result
    end
    

val s = factorial 5

(* 4 *)
fun fixed_point f x = do_until f (fn x => (f x) <> x) x


(* 5 *)
fun map2 f (x,y) = ((f x), (f y))

(* 6 *)
(* val app_all f = List.foldl (fn (x, acc) => acc@(f x)) [] *)

fun app_all f g x = List.foldl (fn (x, acc) => acc@(f x)) [] (g x)

fun g x = [x, x*2, x*3]

val s = app_all g g 1

(* 7 *)
fun foldl f init xs = 
    case xs of
        [] => init |
        x::rest => foldl f (f (x, init)) rest


fun foldr f init xs =
    case xs of
        [] => init |
        x::rest => f (x, foldr f init rest)

val s = foldr (fn(x, acc) => (x+2)::acc) [] [1,2,3]

(* 8 *)
fun partition p xs = foldr (fn (x, (first, second)) => if p x then (x::first, second) else (first, x::second)) ([], []) xs

val s = partition (fn x => x > 4) [1,2,3,4,5,6,7]

(* 9 *)
fun unfold f x =
    case f x of
        NONE => [] |
        SOME (n, m) => n::unfold f m

val s = unfold (fn n => if n < 0 then NONE else SOME(n, n-1)) 9

(* 10 *)
fun fact2 x = List.foldl (fn(n, acc) => n * acc) 1 (unfold (fn n => if n < 1 then NONE else SOME(n, n-1)) x)

val s = fact2 5

(* 11 *)
fun map f xs = List.foldr (fn (x, acc) => (f x)::acc) [] xs

val s = map (fn x => x * 10) [1,2,3]

