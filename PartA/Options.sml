(* An option can have:
 *   - NONE
 *   - SOME expression 
 * as values
 *)

(* fn : int list -> int option *)
fun max(xs: int list) =
  if null xs
  then NONE
  else 
    let 
      val tl_max = max(tl xs)
    in
      if isSome tl_max andalso valOf tl_max > hd xs
      then tl_max
      else SOME(hd xs)
    end

(* The above functions works with options all the way 
 * through the body.
 *
 * It can be rewritten by having an inner function that accepts
 * a non empty list, thus always returning an `int` instead of
 * an option int.
 *
 * The outer function makes sure that it feeds this inner function
 * a non empty list, thus eliminating the need for option
 *)
