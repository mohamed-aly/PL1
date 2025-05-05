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


val s = number_misgraded [(fail, {grade=(SOME 75), id=1}), (pass, {grade=(SOME 80), id=1})]