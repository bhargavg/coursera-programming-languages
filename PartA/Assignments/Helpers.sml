(* Set of helper functions for testing *)
fun triple_option_to_string(maybe_date: (int*int*int) option) =
  if isSome maybe_date
  then 
    let 
      val date = valOf maybe_date
      val y = #1 date
      val m = #2 date
      val d = #3 date
    in
      "(" ^ Int.toString y ^ ", " ^ Int.toString m ^ ", " ^ Int.toString d ^ ")"
    end
  else "NONE"

fun int_list_to_string(xs: int list) =
  let 
    fun formatter(xs: int list) =
      if null xs 
      then "" 
      else Int.toString(hd xs) ^ (if null (tl xs) then "" else ", ") ^ formatter(tl xs)
  in
    "[" ^ formatter xs ^ "]"
  end

fun string_list_to_string(xs: string list) =
  let 
    fun formatter(xs: string list) =
      if null xs 
      then "" 
      else hd xs ^ (if null (tl xs) then "" else ", ") ^ formatter(tl xs)
  in
    "[" ^ formatter xs ^ "]"
  end

fun triple_to_string(x: (int*int*int)) = 
  let 
    val (a, b, c) = x
  in
    "(" ^ Int.toString a ^ ", " ^ Int.toString b ^ ", " ^ Int.toString c ^ ")"
  end

fun triple_list_to_string(xs: (int*int*int) list) =
  let 
    fun formatter(xs: (int*int*int) list) =
      if null xs 
      then "" 
      else triple_to_string(hd xs) ^ (if null (tl xs) then "" else ", ") ^ formatter(tl xs)
  in
    "[" ^ formatter xs ^ "]"
  end


fun test_int(ret_value: int, exp_value: int) = 
  ret_value = exp_value orelse raise Fail("Expectation failed: expected - " ^ Int.toString exp_value ^ ", found - " ^ Int.toString ret_value);

fun test_string(ret_value: string, exp_value: string) = 
  ret_value = exp_value orelse raise Fail("Expectation failed: expected - " ^ exp_value ^ ", found - " ^ ret_value);

fun test_date(ret_value: (int*int*int) option, exp_value: (int*int*int) option) = 
  ret_value = exp_value orelse raise Fail("Expectation failed: expected - " ^ triple_option_to_string exp_value ^ ", found - " ^ triple_option_to_string ret_value);

fun test_int_list(ret_value: int list, exp_value: int list) =
  ret_value = exp_value orelse raise Fail("Expectation failed: expected - " ^ int_list_to_string exp_value ^ ", found - " ^ int_list_to_string ret_value);

fun test_string_list(ret_value: string list, exp_value: string list) =
  ret_value = exp_value orelse raise Fail("Expectation failed: expected - " ^ string_list_to_string exp_value ^ ", found - " ^ string_list_to_string ret_value);

fun test_string_option_list(ret_value: string list option, exp_value: string list option) =
  case (ret_value, exp_value) of
       (NONE, NONE) => true
     | (SOME a, SOME b) => a = b orelse raise Fail("Expectation failed: expected - " ^ string_list_to_string b ^ ", found - " ^ string_list_to_string a)
     | _ => raise Fail("Expectation failed: expected");

fun test_triple_list(ret_value: (int*int*int) list, exp_value: (int*int*int) list) =
  ret_value = exp_value orelse raise Fail("Expectation failed: expected - " ^ triple_list_to_string exp_value ^ ", found - " ^ triple_list_to_string ret_value);

fun test_first_middle_last_string_record(ret_value: {first: string, middle: string,  last: string},
                                         exp_value: {first: string, middle: string,  last: string}) =
  let 
    val {first=ret_first, middle=ret_middle, last=ret_last} = ret_value
    val {first=exp_first, middle=exp_middle, last=exp_last} = exp_value
    val matched = (ret_first=exp_first) andalso (ret_middle=exp_middle) andalso (ret_last=exp_last)
    fun formatter(first: string, middle: string, last: string) =
      "{first=" ^ first ^ ", middle=" ^ middle ^ ", last=" ^ last ^ "}"
  in
    matched orelse (raise Fail("Expectation failed: expected - " ^
    formatter(exp_first, exp_middle, exp_last) ^ ", found - " ^
    formatter(ret_first, ret_middle, ret_last)))
  end

fun test_first_middle_last_string_records(ret_values: {first: string, middle: string,  last: string} list,
                                          exp_values: {first: string, middle: string,  last: string} list) =
  case (ret_values, exp_values) of
       ([], []) => true
     | (x::xs, y::ys) =>
         test_first_middle_last_string_record(x, y) andalso 
         test_first_middle_last_string_records(xs, ys)
     | ([], _::_) => raise Fail("Expectation failed: got less values than expected")
     | (_::_, []) => raise Fail("Expectation failed: got more values than expected")

fun test_bool(ret_value: bool, exp_value: bool) =
  ret_value = exp_value orelse raise Fail("Expectation failed: expected - " ^ Bool.toString exp_value ^ ", found - " ^ Bool.toString ret_value);
