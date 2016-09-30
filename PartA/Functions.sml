fun pow(x: int, y: int) = 
  if y = 0
  then 1
  else x * pow(x, y - 1)

fun cube(x: int) =
  pow(x, 3)


(* Anonymous functions *)
fun n_times(f, n, x) =
  if n = 0
  then x
  else f(n_times(f, n - 1, x))

(* With outer/public helper function *)
fun triple x = 3 * x
fun triple_n_times_1(n, x) = n_times(triple, n, x)

(* With inner/private helper function *)
fun triple_n_times_2(n, x) =
  let 
    fun triple x = 3 * x
  in
    n_times(triple, n, x)
  end

(* With inner/private helper function defined with as let expression *)
fun triple_n_times_3(n, x) = 
  n_times(let fun triple x = 3 * x in triple end, n, x)

(* With inner/private anonymous helper function *)
fun triple_n_times_4(n, x) =
  n_times(fn x => 3 * x, n, x)


(* Map and Reduce *)
fun map(f, xs) =
  case xs of
       [] => []
     | x::xs' => f(x)::map(f, xs')

fun filter(f, xs) =
  case xs of
       [] => []
     | x::x' => if f(x)
                then x::filter(f, x')
                else filter(f, x')


(* Below 2 versions are same *)
val my_fun = fn x => 2 * x
fun my_fun1 x = 2 * x



(* Function composition *) 
fun compose(f, g) = fn x => f(g x)

fun sqrt_of_abs i = Math.sqrt(Real.fromInt(abs i))
fun sqrt_of_abs_composed i = (Math.sqrt o Real.fromInt o abs) i

infix |> 
fun x |> f = f x

fun sqrt_of_abs_composed i = i |> abs |> Real.fromInt |> Math.sqrt


(* Currying *)
val sorted3 = fn x => fn y => fn z => z >= y andalso y >= x 
fun sorted3_1 x =
  fn y => fn z => z >= y andalso y >= x

fun sorted3_nicer x y z = z >= y andalso y >= x


(* curried function vs function taking a tuple *)
fun my_curry x y z = x + y + z
fun my_tuple(x, y, z) = x + y + z

val _ = my_curry 5 4 5 (* See, no braces *)
val _ = my_tuple(5, 4, 5) (* Braces are important here *)


(* Type vars not generalized 
 * This happens because partially applied functions can't be polymorphic
 *)
val pair_with_one = List.map(fn x => (x, 1))
val pair_with_one_working: string list -> (string * int) list = List.map(fn x => (x, 1))



(* Change from function that accepts tuple 
 * to a function that supports currying
 *)
fun range(i, j) = 
  if i > j
  then []
  else i::range(i+1, j)

fun curry f i j = f(i, j)
fun uncurry f (i,j) = f i j

val countup = curry range 1
val one_to_ten = countup 10

