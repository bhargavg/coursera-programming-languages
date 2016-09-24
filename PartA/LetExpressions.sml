fun simpleLetBindings() =
  let 
    val x = 1 
    val y = 2
  in
    x + y
  end

fun nestedLetBindings() =
  let 
    val x = 1
    val y = 2
  in
    (let val x = 3 in if x = 3 then x else 3 end) - 3
  end


(* functions in let bindings *)

fun countup_from1(to: int) =
  let 
    fun count(from: int) =
      if from = to
      then to::[] 
      else from::count(from+1)
  in
    count(1)
  end
