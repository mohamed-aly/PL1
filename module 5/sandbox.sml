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

val s = nth_tail([1,2,3,4], 1)

val rev = List.rev

val z = rev [1,2,3,4]