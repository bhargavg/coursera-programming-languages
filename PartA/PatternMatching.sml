(*
 * variable and function bindings actually take patterns
 *
 * val PATTERN = EXPRESSION
 *
 * fun foo PATTERN
 *)

type triple = int*int*int

fun simple triple =
  case triple of
    (x,y,z) => x+y+z

fun simple_1 triple =
  let 
    val (x, y, z) = triple
  in
    x + y + z
  end

fun simple_2 (x, y, z) =
  x + y + z


type name = {first: string, middle: string, last: string}

fun name_as_string (n: name) = 
  case n of
       {middle=y, first=x, last=z} => x ^ " " ^ y ^ " " ^ z

fun name_as_string_1 n =
  let 
    val {first = x, middle = y, last = z} = n
  in
    x ^ " " ^ y ^ " " ^ z
  end

fun name_as_string_2 {first=x, middle=y, last=z } =
    x ^ " " ^ y ^ " " ^ z



(* Implement [un]zip3 function *)
fun zip3 (xs, ys, zs) = 
  case (xs, ys, zs) of
       (x::xs, y::ys, z::zs) => (x, y, z) :: zip3(xs, ys, zs)
     | _ => []

fun unzip3(xs) =
  case xs of
       ((x,y,z)::xs) => let 
                          val (a, b, c) = unzip3(xs)
                        in 
                          (x::a, y::b, z::c)
                        end
     | _ => ([], [], [])


fun nondecreasing xs =
  case xs of
       [] => true
     | (_::[]) => true
     | (x::y::xs) => y <= x andalso nondecreasing xs


(* Get the sign of multiplication result *) 
datatype sgn = Positive | Negative | Zero
fun multsign(x, y) =
  let fun sign x = if x=0 then Zero else if x > 0 then Positive else Negative
  in
    case (sign x, sign y) of
        (Zero, _)  => Zero
       | (_, Zero) => Zero
       | (Positive, Positive) => Positive
       | (Negative, Negative) => Positive
       | (Positive, Negative) => Negative
       | (Negative, Positive) => Negative
  end


(* Compute the length of the list *)
fun len xs =
  case xs of
       [] => 0
     | _::xs => 1 + len xs


(* Compute the max element in a list *)
fun max_in_list xs = 
  case xs of
       [] => 0
     | x::[] => x
     | x::xs => Int.max(x, max_in_list xs)

(* Compute the reverese of a list *)
fun xrev xs =
  case xs of
       [] => []
     | x::xs => xrev(xs) @ [x]

fun xrev_tail xs = 
  let
    fun aux(xs, acc) =
      case xs of
           [] => acc
         | x::xs => aux(xs, x::acc)
  in 
    aux(xs, [])
  end


(*
 * Another way to write functions with case expressions is as follows
 *
 * fun f x =
 *   case x of
 *        p1 = e1
 *      | p2 = e2
 *      | p3 = e3
 *
 * fun f p1 = e1
 *   | f p2 = e2
 *   | f p3 = e3
 *)
