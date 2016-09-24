use "PartA/Assignments/02_Week.sml";

fun date_to_string_option(maybe_date: (int*int*int) option) =
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

fun date_to_string(date: (int*int*int)) =
  "(" ^ Int.toString(#1 date) ^ ", " ^ Int.toString(#2 date) ^ ", " ^ Int.toString(#3 date) ^ ")"

fun list_to_string(xs: int list) =
  let 
    fun formatter(xs: int list) =
      if null xs 
      then "" 
      else Int.toString(hd xs) ^ (if null (tl xs) then "" else ", ") ^ formatter(tl xs)
  in
    "[" ^ formatter xs ^ "]"
  end


fun test_int(ret_value: int, exp_value: int) = 
  ret_value = exp_value orelse raise Fail("Expectation failed: expected - " ^ Int.toString exp_value ^ ", found - " ^ Int.toString ret_value);

fun test_string(ret_value: string, exp_value: string) = 
  ret_value = exp_value orelse raise Fail("Expectation failed: expected - " ^ exp_value ^ ", found - " ^ ret_value);

fun test_date(ret_value: (int*int*int) option, exp_value: (int*int*int) option) = 
  ret_value = exp_value orelse raise Fail("Expectation failed: expected - " ^ date_to_string_option exp_value ^ ", found - " ^ date_to_string_option ret_value);

fun test_list(ret_value: int list, exp_value: int list) =
  ret_value = exp_value orelse raise Fail("Expectation failed: expected - " ^
  list_to_string exp_value ^ ", found - " ^ list_to_string ret_value);






test_int(number_before_reaching_sum(5,  [3,2,2]),     1);
test_int(number_before_reaching_sum(6,  [4,1,1,1]),   2);
test_int(number_before_reaching_sum(10, [1,2,3,4,5]), 3);

test_string(get_nth(["hi"],           1),  "hi");
test_string(get_nth(["1", "2", "3"],  3),   "3");
test_string(get_nth(["1", "2", "3"],  2),   "2");
test_string(get_nth(["1", "2", "3"],  1),   "1");
test_string(get_nth(["0", "1"],       1),   "0");
test_string(get_nth(["0", "1"],       2),   "1");

test_int(what_month(31),    1);
test_int(what_month(32),    2);
test_int(what_month(59),    2);
test_int(what_month(365),  12);

test_date(oldest([(5,5,2),(5,10,2),(5,2,2),(5,12,2)]), SOME (5,12,2));

test_list(month_range(31,32), [1,2]);
test_list(month_range(335, 365), [12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12]);
test_list(month_range(85,145), [3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5]);
test_list(month_range(1,31), [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]);
test_list(month_range(1,365), [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12]);
