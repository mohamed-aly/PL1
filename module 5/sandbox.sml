(* Introduction to First-Class Functions *)

fun double x = 2 * x
fun incr x = x + 1
val a_tuble = (double, incr, double(incr 7))
val s = (#1 a_tuble 3)

(* Polymorphic Types and Functions as Arguments *)
fun incr x = x + 1
fun double x = x * 2
fun n_times(f, x, n) = if n = 0 then x else f(n_times(f,x, n-1))

val x = n_times(incr, 3, 3)