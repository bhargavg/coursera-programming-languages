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



(* Imaginary language grammer *)
datatype exp = Constant of int
             | Negate of exp
             | Add of exp * exp
             | Subtract of exp * exp
             | Multiply of exp * exp
             | Divide of exp * exp

fun eval e = 
  case e of
       Constant x => x
     | Negate x => ~ (eval x)
     | Add (x, y) => (eval x) + (eval y)
     | Subtract(x, y) => (eval x) - (eval y)
     | Multiply(x, y) => (eval x) * (eval y)
     | Divide(x, y) => (eval x) div (eval y)

val x = eval(Add(Constant 5, Add(Constant 5, Negate( Constant 6))))
