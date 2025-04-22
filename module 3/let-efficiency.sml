
fun count_up(from: int, to: int) = 
    if from > to
    then []
    else from :: count_up(from+1, to)

fun count_down(from: int, to: int) = 
    if from < to
    then []
    else from :: count_down(from-1, to)

fun bad_max(ls: int list) = 
    if null ls
    then 0
    else if null (tl ls)
    then (hd ls)
    else if (hd ls) > bad_max(tl ls)
    then (hd ls)
    else bad_max(tl ls)

fun good_max(ls: int list) = 
    if null ls
    then 0
    else if null (tl ls)
    then (hd ls)
    else
        let
            val hd_ans = (hd ls)
            val tl_ans = good_max(tl ls)
        in
            if hd_ans > tl_ans
            then hd_ans
            else tl_ans
        end