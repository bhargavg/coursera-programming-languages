datatype mytype = TwoInts of int * int
                | Str of string
                | Pizza

fun f x = 
  case x of
       Pizza => 3
     | Str s => 4
     | TwoInts (i1, i2) => 5



(* Model cards *)
datatype suite = Club
               | Diamond
               | Heart
               | Spade

datatype rank  = Jack
               | King
               | Queen
               | Ace
               | Num of int

datatype card  = Card of suite * rank
type card_type = suite * rank



(* Imaginary language grammer *)
datatype exp = Const of int
             | Neg of exp

fun eval e = 
  case e of
       Const x => x
     | Neg x => ~ (eval x)

val x = eval(Const 5)



(* Find largest constant in the expression *)
datatype expression = Constant of int
                    | Negate of expression
                    | Add of expression * expression
                    | Multiply of expression * expression

fun evaluate e = 
  case e of
       Constant x => x
     | Negate x   => ~ (evaluate x)
     | Add(x, y)  => (evaluate x) + (evaluate y)
     | Multiply(x, y) => (evaluate x) * (evaluate y)

fun max_constant e =
  let 
    fun max_of_expressions(e1: expression, e2: expression) =
      Int.max( max_constant(e1), max_constant(e2) )
  in
    case e of 
         Constant x => x
       | Negate x   => max_constant x
       | Add (x, y) => Int.max( max_constant x, max_constant y)
       | Multiply(x,y) => Int.max( max_constant x, max_constant y)
  end

val test_exp = Add(Constant ~9, Negate (Constant ~4));




(* Defining our own List type 
 * List can be defined recursively as follows: 
 *)
datatype IntList = Empty
                 | Cons of int * IntList

fun append(xs: IntList, ys: IntList) =
  case xs of
       Empty => ys
     | Cons(head, tail) => Cons(head, append(tail, ys))

val x_list = Cons(2, Cons(1, Empty))
val y_list = Cons(5, Cons(6, Empty))

