val x = (true, 1)

(* Access with #1, #2,...#n *)
(* #1 x = true *)

fun swap(pr: int*bool) = 
  (#2 pr, #1 pr)
