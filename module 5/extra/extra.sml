fun compose_opt f g x = 
    case g x of 
        NONE => NONE |
        SOME X => f x

fun safeSqrt n = if n < 0 then NONE else SOME(Math.sqrt( real n))

fun halfIfEven n = if n mod 2 = 0 then SOME(n div 2) else NONE

val h = compose_opt halfIfEven safeSqrt

val s = h 16

fun do_until f p x = 
    let
        val result = f x
    in
        if not (p(result)) then do_until f p result else result
    end

val s = do_until (fn x => x div 2) (fn x => x mod 2 <> 1) 99

