(* this is a cooment *)
fun pow(x: int, y: int) = 
    if y = 0 
        then 1
    else
        x * pow(x, y-1);

fun cude(x: int) = pow(x, 3)