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


