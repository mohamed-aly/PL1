(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "hw3provided.sml";

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test2 = longest_string1 ["A","bc","CC"] = "bc"

val test3 = longest_string2 ["A","bc","CC"] = "CC"

val test4a = longest_string3 ["A","bc","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A"

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test8a = all_answers (fn x => if x > 1 then SOME [x] else NONE) [2,3,4,5,6,7] = SOME [2,3,4,5,6,7]

val test9a = count_wildcards Wildcard = 1

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

val test9c = count_some_var ("x", Variable("x")) = 1

val test10 = check_pat (Variable("x")) = true

val test11 = match (Const(1), UnitP) = NONE

val test11_0  = match (Const(1), UnitP) = NONE

val test11_1  = match (Const(42), Wildcard) = SOME []
val test11_2  = match (Unit, Variable("x")) = SOME [("x", Unit)]
val test11_3  = match (Unit, UnitP) = SOME []
val test11_4  = match (Const(5), ConstP(5)) = SOME []
val test11_5  = match (Const(6), ConstP(5)) = NONE

val test11_6  = match (Tuple [Const(1)], TupleP [Variable("x"), UnitP]) = NONE
val test11_7  = match (Tuple [Const(1), Unit], TupleP [Variable("a"), UnitP]) = SOME [("a", Const(1))]
val test11_8  = match (Tuple [Const(1), Unit], TupleP [Variable("a"), ConstP(2)]) = NONE
val test11_9  = match (Tuple [Tuple [Const(3), Const(4)], Const(9)],
                        TupleP [TupleP [Wildcard, Variable("y")], Variable("z")])
                   = SOME [("y", Const(4)), ("z", Const(9))]

val test11_10 = match (Constructor("Some", Const(7)),
                        ConstructorP("Some", Variable("v")))
                   = SOME [("v", Const(7))]
val test11_11 = match (Constructor("None", Unit),
                        ConstructorP("Some", Wildcard))
                   = NONE
val test11_12 = match (Constructor("Zero", Unit),
                        ConstructorP("Zero", UnitP))
                   = SOME []

val test11_13 = match (Tuple [Constructor("Some", Const(1)), Constructor("Ok", Unit)],
                        TupleP [ConstructorP("Some", Variable("x")), ConstructorP("Ok", UnitP)])
                   = SOME [("x", Const(1))]

val test11_14 = match (Constructor("Pair",
                                   Tuple [Constructor("Some", Const(2)),
                                          Constructor("Some", Const(3))]),
                        ConstructorP("Pair",
                                     TupleP [ConstructorP("Some", Variable("a")),
                                             ConstructorP("Some", Variable("b"))]))
                   = SOME [("a", Const(2)), ("b", Const(3))]

val test11_15 = match (Constructor("Pair",
                                   Tuple [Constructor("Some", Const(2)),
                                          Constructor("Some", Const(3))]),
                        ConstructorP("Pair",
                                     TupleP [ConstructorP("Some", Variable("a")),
                                             ConstructorP("None", Wildcard)]))
                   = NONE

val test11_16 = match (Tuple [Const(10), Unit],
                        TupleP [ConstP(10), UnitP])
                   = SOME []

val test11_17 = match (Constructor("Just", Tuple [Const(1), Unit]),
                        ConstructorP("Just", TupleP [Wildcard, UnitP]))
                   = SOME []

(* val test12 = first_match Unit [UnitP] = SOME [] *)

