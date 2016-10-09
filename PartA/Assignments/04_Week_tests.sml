(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "PartA/Assignments/04_Week.sml";

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test2a = longest_string1 ["A","bc","C"] = "bc"
val test2b = longest_string1 ["A1","bc","C"] = "A1"

val test3a = longest_string2 ["A","bc","C", "cd"] = "cd"
val test3b = longest_string2 ["A","bc","cd", "e"] = "cd"

val test4a = longest_string3 ["A","bc","C"] = "bc"
val test4b = longest_string3 ["A1","bc","C"] = "A1"

val test4c = longest_string4 ["A","bc","C", "cd"] = "cd"
val test4d = longest_string4 ["A","bc","cd", "e"] = "cd"

val test5a = (longest_capitalized ["A","bc","C"]) = "A"

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test9a1 = count_wildcards Wildcard = 1
val test9a2 = count_wildcards (Variable "hi") = 0
val test9a3 = count_wildcards (TupleP [Variable "hello", Wildcard, UnitP, TupleP [ConstructorP ("world", Wildcard)]]) = 2

val test9b1 = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b2 = count_wild_and_variable_lengths (UnitP) = 0
val test9b3 = count_wild_and_variable_lengths (TupleP [Variable "hello", Wildcard, UnitP, TupleP [ConstructorP ("world", Wildcard)]]) = 2 + 5

val test9c1 = count_some_var ("x", Variable("x")) = 1
val test9c2 = count_some_var ("x", Wildcard) = 0
val test9c3 = count_some_var ("x", UnitP) = 0

val test10a = check_pat (Variable("x")) = true
val test10b = check_pat (TupleP [Wildcard, Variable "x", TupleP [Variable "x"]]) = true
val test10c = check_pat (TupleP [Wildcard, Variable "x", TupleP [Variable "y"]]) = false

val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME []
