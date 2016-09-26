(* Problem: Write a function all_except_option, which takes a
 * string and a string list. Return NONE if the string is not 
 * in the list, else return SOME lst where lst is identical to 
 * the argument list except the string is not in it. You may 
 * assume the string is in the list at most once. Use same_string,
 * provided to you, to compare strings. Sample solution is around 8 lines.
 *)
fun all_except_option(x: string, xs: string list) =
  let 
    fun aux(x: string, xs: string list, found: bool) =
      case xs of 
           [] => ([], found)
         | a::[] => if a=x then ([], true) else (a::[], found)
         | a::a' => if a=x
                    then aux(x, a', true)
                    else
                      let 
                        val (tail, found) = aux(x, a', found)
                      in 
                        (a::tail, found)
                      end
  in
    let val (list, found) = aux(x, xs, false)
    in
      if found
      then SOME list
      else NONE
    end
  end


(* Problem: Write a function get_substitutions1, which takes a string list list
 * (a list of list of strings, the substitutions and a string s and returns a
 * string list. The result has all the strings that are in some list in substitutions
 * that also has s, but s itself should not be in the result. 
 *
 * Example:
 *  get_substitutions1(
 *    [
 *      ["Fred","Fredrick"],
 *      ["Elizabeth","Betty"],
 *      ["Freddie","Fred","F"]
 *    ],
 *    "Fred"
 *  );
 *
 *  (* answer: ["Fredrick","Freddie","F"] *)
 *)
fun get_substitutions1(xss: string list list, x: string) =
  case xss of
       [] => []
     | head::tail => case all_except_option(x, head) of
                          NONE => [] @ get_substitutions1(tail, x)
                        | SOME x_list => x_list @ get_substitutions1(tail, x)
