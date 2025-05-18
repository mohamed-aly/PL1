(* Introduction to First-Class Functions *)

fun double x = 2 * x
fun incr x = x + 1
val a_tuble = (double, incr, double(incr 7))

(* Polymorphic Types and Functions as Arguments *)
fun incr x = x + 1
fun double x = x * 2
fun n_times(f, x, n) = if n = 0 then x else f(n_times(f,x, n-1))

(* Anonymous Functions *)
fun triple_n_times (x, n) = n_times(fn x => x * 3, x, n)

(* Unnecessary Function Wrapping *)
fun nth_tail (xs, n) = n_times(tl, xs, n)

val rev = List.rev

(* Map and Filter *)
fun map(f, xs) =
    case xs of
        [] => [] |
        x::rest => (f x)::map(f, rest)

val s = map(hd, [[1,2], [3,4], [5,6]])
val s = map(incr, [1,2,3,4,5])

fun filter(f, xs) =
    case xs of
        [] => [] |
        x::rest => if f x then x::filter(f, rest) else filter(f, rest)

fun is_even n = (n mod 2 = 0)
val all_even = filter(is_even, [1,2,3,4,5,6])

(* Generalizing Prior Topics *)

(* Lexical Scope *)
val x = 1

fun f y = y + x

val x = 2

val y = 3

val z = f (x + y)

(* Lexical Scope and Higher-Order Functions *)
val x = 1

fun f y =
    let
        val x = y + 1
    in
        fn z => x + y + z
    end

val s = f 4

val g = s 6

(* Why Lexical Scope *)

fun greaterThanX xs = fn y => y > xs

fun nonNegatives xs = filter(greaterThanX ~1, xs)

fun allGreater(xs, n) = filter(fn x => x > n, xs)

(* Closures and Recomputation *)

fun allShorterThan1(xs, s) = filter(fn x => String.size x < String.size s, xs)

fun allShorterThan2(xs, s) = 
    let
        val i = String.size s
    in
        filter(fn x => String.size x < i, xs)
    end

(* Fold and More Closures *)

fun fold(f, xs, acc) = 
    case xs of
        [] => acc |
        x::rest => fold(f, rest, f(acc, x))

fun f1 xs = fold(fn (x,y) => x+y, xs, 0)

fun f2 xs = fold(fn (x, y) => x andalso y >= 0, xs, true)

fun f3 (xs, lo, hi) = fold(fn (x, y) => x + (if y >= lo andalso y <= hi then 1 else 0), xs, 0)

val s = f1 [1,2,3,4]

val x = f2 [~1,2,3,4]