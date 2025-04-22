fun old_max(xs: int list) = 
    if null xs
    then 0
    else if null (tl xs)
    then hd xs
    else
        let 
            val hd_ans = hd xs
            val tl_ans = old_max(tl xs)
        in
            if hd_ans > tl_ans
            then hd_ans
            else tl_ans
        end


fun max1(xs: int list) = 
    if null xs
    then NONE
    else
        let
            val hd_value = hd xs
            val tl_option = max1(tl xs)
        in
            if isSome tl_option andalso valOf tl_option > hd_value
            then tl_option
            else SOME(hd_value) 
        end


fun max2(xs: int list) = 
    if null xs
    then NONE
    else
        let
            fun max_nonempty(xs: int list) = 
                if null (tl xs)
                then hd xs
                else
                    let
                        val tl_ans = max_nonempty(tl xs)
                    in
                        if hd xs > tl_ans
                        then hd xs
                        else tl_ans
                    end
        in
            SOME(max_nonempty xs)
        end