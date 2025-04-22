fun silly(x: int) = 
    let
      val y = 10
    in
      x + y
    end

fun silly2() = 
    let
      val x = 2
    in
      ((let
        val x = 5
      in
        x
      end), x)
    end