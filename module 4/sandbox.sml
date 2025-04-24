datatype exp = Constant of int |
 Negate of exp |
  Add of exp * exp |
   Multiply of exp * exp
val v = Add (Constant(12+3), Negate (Constant 4))

fun eval e = 
    case e of 
        Constant i => i
        | Negate ex => ~ (eval ex)
        | Add (e1, e2) => (eval e1) + (eval e2)
        | Multiply (e1, e2) => (eval e1) * (eval e2)

fun max_constant e = 
    case e of
        Constant i => i
        | Negate ex => max_constant ex
        | Add (e1, e2) => Int.max(max_constant e1, max_constant e2)
        | Multiply (e1, e2) => Int.max(max_constant e1, max_constant e2)


datatype suit = Club | Heart | Diamond | Spade
datatype rank = Jack | Queen | King | Num of int

type card = suit * rank

val s = Club
val r = Jack

val c: card = (s,r)

fun is_queen_of_spades(c: card) = 
    #1 c = Spade andalso #2 c = Queen

fun is_queen_of_spade(c: card) = 
    case c of 
        (Spade, Queen) => true
        | _ => false

(* Lists and Options are Datatypes *)

datatype int_list = Empty | Cons of int * int_list

fun append_to_list(xs: int_list, ys: int_list) = 
    case xs of 
        Empty => ys
        | Cons(x,xs') => Cons(x, append_to_list(xs', ys))

val x = Cons(1, Cons(2, Cons(3, Cons(4, Empty))))

val s = append_to_list(Cons(0, Empty), x)

fun sum_list xs = 
    case xs of
        [] => 0 |
        x::xs' => x + sum_list xs'

fun append([], ys) = ys |
    append(x::xs, ys) = x::append(xs, ys)

val p = append([1,2,3,4], [5,6,7])

(* Polymorphic Datatypes *)
datatype 'a option = NONE | SOME of 'a

datatype ('a, 'b) tree = Node of 'a * ('a, 'b) tree * ('a, 'b) tree | Leaf of 'b

fun sum_tree(Leaf i) = i |
    sum_tree(Node(i, lft, rgt)) = i + sum_tree(lft) + sum_tree(rgt)

fun sum_leaves(Leaf i) = i |
    sum_leaves(Node(i, lft, rgt)) = sum_leaves lft + sum_leaves rgt

fun num_nodes(Leaf i) = 0 |
    num_nodes(Node(i, lft, rgt)) = 1 + num_nodes lft + num_nodes rgt

fun num_leaves(Leaf i) = 1 |
    num_leaves(Node(i, lft, rgt)) = num_leaves lft + num_leaves rgt

(* Each of Pattern Matching / Truth About Functions *)
fun sum_triple(x,y,z) = x + y + z

fun fullname {first=x, middle=y, last=z} = x ^ " " ^ y ^ " " ^ z

(* Nested Patterns *)
fun zip3([], [], []) = [] |
    zip3(x::xs, y::ys, z::zs) =
        (x,y,z)::zip3(xs,ys,zs)

fun unzip3([]) = ([], [], []) |
    unzip3((x,y,z)::rest) = 
        let
          val (xs,ys,zs) = unzip3(rest)
        in
          (x::xs, y::ys, z::zs)
        end

(* More Nested Patterns *)
fun nondecreasing xs = 
    case xs of
        [] => true |
        x::[] => true |
        x::y::rest => x <= y andalso nondecreasing(y::rest)

datatype sgn = Z | P | N

fun multsgn (x,y) =
    let
      fun sign n = if n = 0 then Z else if n > 0 then P else N
    in
      case (sign x, sign y) of
        (Z, _) => Z |
        (_, Z) => Z |
        (P, P) => P |
        (N, N) => P |
        _ => N 
    end

(* Exceptions *)
exception DivideByZero

fun mydiv(x,y) = 
    if y = 0 then raise DivideByZero else x div y

fun maxList(xs, ex) = 
    case xs of
        [] => raise ex |
        x::[] => x |
        x::xs' => Int.max(x, maxList(xs', ex))

val v = maxList([], List.Empty) handle List.Empty => 0

(* Tail Recursion *)

fun fact n = if n = 0 then 1 else n*fact(n-1)

fun fact2 n = 
    let
      fun aux(n, acc) = if n=0 then acc else aux(n-1, n*acc)
    in
      aux(n,1)
    end

(* Accumulators for Tail Recursion *)

fun sum_tail n = 
    let
        fun aux(n, acc) = 
            if n=0 then acc else aux(n-1, n+acc)
    in
        aux(n,0)
    end

fun reverse(xs) = 
    case xs of
        [] => [] |
        x::xs' => (reverse xs') @ [x] 

fun reverse_tail(xs) = 
    let
        fun aux(xs, acc) = 
            case xs of
                [] => acc |
                x::xs' => aux(xs', x::acc)
    in
        aux(xs, [])
    end

val s = reverse_tail([1,2,3,4])