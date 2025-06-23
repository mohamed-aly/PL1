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

(* Closure Idiom: Combining Functions *)

fun sqrt_of_abs i = Math.sqrt(Real.fromInt(abs i))

fun sqrt_of_abs2 i = (Math.sqrt o Real.fromInt o abs) i

val sqrt_of_abs3 = Math.sqrt o Real.fromInt o abs

val s = sqrt_of_abs3 ~16

infix |>

fun x |> f = f x

fun sqrt_of_abs4 i = i |> abs |> Real.fromInt |> Math.sqrt

val s = sqrt_of_abs4 ~16

(* Currying *)

val sorted = fn x => fn y => fn z => z >= y andalso y >= x

val s = (((sorted 7) 9) 11)

fun sorted_nicer x y z = z >= y andalso y >= x

val s = sorted_nicer 5 6 7

(* Partial Application *)

val is_nonnegative = sorted_nicer 0 0

val s = is_nonnegative ~1

fun range i j = if i > j then [] else i :: range (i + 1) j

val countup = range 1

val s =  countup 10

(* Currying Wrapup *)

fun range2 (i, j) = if i > j then [] else i :: range2(i+1, j)

fun curry f x y = f (x,y)

fun uncurry f(x,y) = f x y

val countup2 = curry range2 1

val s = countup2 7

(* Mutable References *)

val x = ref 42
val y = ref 42
val z = x
val _ = x := 43

(* Optional: Closure Idioms Without Closures *)

datatype 'a mylist = Cons of 'a * ('a mylist) | Empty

fun map f xs = case xs of
    Empty => Empty |
    Cons(x, rest) => Cons(f x, map f rest)

fun filter f xs = case xs of
    Empty => Empty |
    Cons(x, rest) => if f x then Cons(x, filter f rest) else filter f rest

fun length xs = 
    case xs of
        Empty => 0 |
        Cons(x, rest) => 1 + length rest

val doubleAll = map (fn x => x * 2)

fun countNs (xs, n: int) = length( filter (fn x => x=n) xs)

val s = Cons(3, Cons(4, Cons(5, Empty)))

val x = doubleAll s

val z = countNs(s, 3)



