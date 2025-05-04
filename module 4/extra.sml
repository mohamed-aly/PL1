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


val s = number_passed [{grade=(SOME 75), id=1}, {grade=(SOME 80), id=1}]