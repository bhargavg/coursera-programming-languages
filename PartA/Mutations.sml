(*
 *    ----    ----
 *   | 42 |  | 42 |
 *    ----    ----
 *     ^ ^     ^
 *     | |     |
 *     x z     y
 *)
val x = ref 42
val y = ref 42
val z = y;

x := 50;

val w = (!x) + (!y)
