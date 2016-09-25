
exception MyExceptionWithNoData

exception MyExceptionWithData of int*int

val x = (raise MyExceptionWithData(5, 4)) handle MyExceptionWithData(x, y) => x*y

(* e1 handle ex => e2 *)
(* All custom extensions are just constructors of exn datatype *)
