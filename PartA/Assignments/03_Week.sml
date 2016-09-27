(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
*    string), then you avoid several of the functions in problem 1 having
*       polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
      s1 = s2

(* put your solutions for problem 1 here *)

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
    let val (result_list, found) = aux(x, xs, false)
    in
      if found
      then SOME result_list
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


(* Problem: Write a function get_substitutions2, which is like get_substitutions1 
 * except it uses a tail-recursive local helper function.
 *)
fun get_substitutions2(xss: string list list, x: string) =
  let 
    fun aux(xss: string list list, x: string, acc: string list) =
      case xss of
           [] => acc
         | head::tail => case all_except_option(x, head) of
                              NONE => aux(tail, x, acc)
                            | SOME x_list => aux(tail, x, acc @ x_list)
  in
    aux(xss: string list list, x: string, [])
  end


(* Problem: Write a function similar_names, which takes a string list list 
 * of substitutions (as in parts (b) and (c)) and a full name of type 
 * {first:string,middle:string,last:string} and returns a list of full names 
 * (type {first:string,middle:string,last:string} list). 
 *
 * The result is all the full names you can produce by substituting for the 
 * first name (and only the first name) using substitutions and parts (b) or (c). 
 * The answer should begin with the original name (then have 0 or more other names). 
 *
 * Example: 
 * similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], 
 *               {first="Fred", middle="W", last="Smith"})
 *
 * answer: [{first="Fred", last="Smith", middle="W"},
 *          {first="Fredrick", last="Smith", middle="W"},
 *          {first="Freddie", last="Smith", middle="W"},
 *          {first="F", last="Smith", middle="W"}] 
 *
 * Do not eliminate duplicates from the answer. Hint: Use a local helper function.
 *)
fun similar_names(xss: string list list, 
                  full_name: {first: string, middle: string, last: string}) =
  let
    val {first=first_name, middle=middle_name, last=last_name} = full_name
    val first_names = get_substitutions2(xss, first_name)

    fun aux(full_name: {first: string, middle: string, last: string},
            first_names: string list) =
      case first_names of
           [] => [] 
         | first_name::first_names_tail => {first=first_name, last=last_name, middle=middle_name} :: aux(full_name, first_names_tail)
  in
    full_name::aux(full_name, first_names)
  end







(* you may assume that Num is always used with values 2, 3, ..., 10
*    though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* Problem: Write a function card_color, which takes a card and 
 * returns its color (spades and clubs are black, 
 * diamonds and hearts are red).
 *)
fun card_color card =
  case card of
       (Clubs, _) => Black
     | (Spades, _) => Black
     | (Diamonds, _) => Red
     | (Hearts, _) => Red

(* Problem: Write a function card_value, which takes a card and 
 * returns its value (numbered cards have their number as the value, 
 * aces are 11, everything else is 10).
 *)
fun card_value card =
  case card of
       (_, Num value) => value
     | (_, Ace) => 11
     | _ => 10

(* Problem: Write a function remove_card, which takes a list of cards cs,
 * a card c, and an exception e. It returns a list that has all the elements 
 * of cs except c. If c is in the list more than once, remove only the first one. 
 * If c is not in the list, raise the exception e. You can compare cards with =.
 *)
fun remove_card(cs, c, e) =
  let
    fun aux(cs, c, already_removed) =
      case cs of
           [] => if already_removed then [] else raise e
         | x::x' => if already_removed
                    then x::aux(x', c, already_removed)
                    else if c = x
                         then aux(x', c, true)
                         else x::aux(x', c, false)
  in
    aux(cs, c, false)
  end

(* Problem: Write a function all_same_color, which takes a list of cards 
 * and returns true if all the cards in the list are the same color. 
 * Hint: An elegant solution is very similar to one of the functions 
 * using nested pattern-matching in the lectures.
 *)
