fun sum_list(l: int list) = 
    if null l
    then 0
    else hd l + sum_list(tl l)

fun list_product(ls: int list) = 
    if null ls
    then 0
    else hd ls * list_product(tl ls)

fun countdown(x: int) = 
    if x = 0
    then []
    else x :: countdown(x-1)

fun append(xs: int list, ys: int list) = 
    if null xs
    then ys
    else (hd xs) :: append(tl xs, ys)

fun sum_pairs(xs: (int * int) list) = 
    if null xs
    then 0
    else (#1 (hd xs)) + (#2 (hd xs)) + sum_pairs(tl xs)

fun firsts(xs: (int * int) list) = 
    if null xs
    then []
    else #1 (hd xs) :: firsts(tl xs)

fun seconds(xs: (int * int) list) = 
    if null xs
    then []
    else #2 (hd xs) :: seconds(tl xs)

fun sum_pairs_2(xs: (int * int) list) = 
    sum_list(firsts xs) + sum_list(seconds xs)

