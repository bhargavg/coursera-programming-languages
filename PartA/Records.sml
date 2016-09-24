val x = { bar = (1,2+3,"hi"), foo = 3+4, baz = (false, 9) };
val bar_value  = #bar x;

val my_tuple = { 1 = "hi", 2 = "hello", 3 = "hey" };
val first_value = #1 my_tuple;

(* 
 * Basically, 
 * (e1, e2, e2...en) is same as { 1=e1, 2=e2,...n=en } 
 *)
