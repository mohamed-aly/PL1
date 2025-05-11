type student_id = int
type grade = int (* must be in 0 to 100 range *)
type final_grade = { id : student_id, grade : grade option }
datatype pass_fail = pass | fail

fun pass_or_fail {grade,id} = 
    case grade of 
        SOME i => if i >=75 then pass else fail |
        _ => fail

fun has_passed fg = 
    pass_or_fail fg = pass

fun number_passed fgs = 
    case fgs of
        [] => 0 |
        fg::rest => let val curr = if (has_passed fg) then 1 else 0 in 
                        curr + number_passed rest
                    end

fun number_misgraded(labels) =
    case labels of
        [] => 0 |
        (pf, grade)::ls => if (pass_or_fail grade) = pf then 0 + number_misgraded (ls) else 1 + number_misgraded (ls) 


datatype 'a tree = leaf 
                 | node of { value : 'a, left : 'a tree, right : 'a tree }
datatype flag = leave_me_alone | prune_me

fun tree_height(tree) = 
    case tree of
        leaf => 0 |
        node {value, left, right} =>
            let
                val left_height = tree_height left
                val right_height = tree_height right
            in
                if left_height > right_height then left_height + 1 else right_height + 1
            end

fun sum_tree(tree) = 
    case tree of
        leaf => 0 |
        node {value, left, right} => value + sum_tree left + sum_tree right


fun gardener(flag_tree) =
    case flag_tree of 
        leaf => flag_tree |
        node {value, left, right} => if value = prune_me then leaf else node {value=value, left=gardener left, right= gardener right}


(* Options *)

datatype 'a option = NONE | SOME of 'a
exception Option

fun getOpt(opt, a) = 
    case opt of
        NONE => a |
        SOME v => v

fun isSome opt = 
    case opt of
        SOME _ => true |
        _ => false

fun valOf opt = 
    case opt of 
        SOME v => v |
        _ => raise Option

(* list *)

exception Empty

fun last l =
    case l of
        [] => raise Empty |
        x::[] => x |
        x::xs => last xs


fun length l =
    case l of
        [] => 0 |
        x::xs => 1 + length xs

fun take (l, i) = 
    if i < 0
    then raise Subscript
    else 
    case (l, i) of
        (_, 0) => [] |
        ([], n) => raise Subscript |
        (x::xs, n) => x::take(xs, n-1)

fun drop (l, i) = 
    if i < 0
    then raise Subscript
    else 
    case (l, i) of
        ([], _) => raise Subscript |
        (xs, 0) => xs |
        (_::xs, n) => drop(xs, n-1)

fun concat(ls) = 
    case ls of
        [] => [] |
        l::rest => l @ concat rest

(* part 4 *)

datatype nat = ZERO | SUCC of nat

fun is_positive(n) =
    case n of
        ZERO => false |
        SUCC _ => true

val s = is_positive (SUCC(ZERO))

