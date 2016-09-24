val x = [1,2,3]

(* :: = cons operator *)

val y = 2 :: x

val z = [1,2] :: [[3,4], [5,6]]


(* null to check if list is empty *)
val isZEmpty = null z

(* [] = a' list *)
(* because no type is specified, it can be consed with a type which returns a list of that type *)
val a = 2::[] 

(* hd : a' list -> a' *)
(* tl : a' list -> a' list *)


fun sum_list(xs: int list) = 
  if null xs
  then 0
  else hd xs + sum_list(tl xs)


fun countdown(x: int) =
  if x = 0
  then [0]
  else x::countdown(x-1)

fun count(from: int, to: int) = 
  if from > to
  then []
  else if from = to
  then [to]
  else from :: count(from + 1, to)

fun append(xs: int list, ys: int list) =
  if null xs
  then ys
  else hd xs :: append(tl xs, ys)

fun firsts(xs: (int * int) list) = 
  if null xs
  then []
  else #1 (hd xs) :: firsts(tl xs)

fun seconds(xs: (int * int) list) =
  if null xs
  then []
  else #2 (hd xs) :: seconds(tl xs)

fun sum_list_pairs(xs: (int * int) list) = 
  sum_list(firsts xs) + sum_list(seconds xs)
