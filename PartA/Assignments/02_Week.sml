(* Problem:
 *  Write a function is_older that takes two dates and evaluates to true or
 *  false. It evaluates to true if the first argument is a date that comes 
 *  before the second argument. If the two dates are the same, the result is false.
 *)
fun is_older(date1: int*int*int, date2: int*int*int) =
  let 
    val y1 = #1 date1
    val m1 = #2 date1
    val d1 = #3 date1
    val y2 = #1 date2
    val m2 = #2 date2
    val d2 = #3 date2
  in
    (y1 = y2 andalso m1 = m2 andalso d1 < d2)
    orelse
    (y1 = y2 andalso m1 < m2)
    orelse
    (y1 < y2)
  end


(* Problem: 
 * Write a function number_in_month that takes a list of dates and a month
 * (i.e., an int) and returns how many dates in the list are in the given month.
 *)
fun number_in_month(dates: (int*int*int) list, month: int) =
  if null dates
  then 0
  else 
    let 
      val count = if #2 (hd dates) = month then 1 else 0
    in
      count + number_in_month(tl dates, month)
    end

(* Problem:
 * Write a function number_in_months that takes a list of dates and a list of months 
 * (i.e., an int list) and returns the number of dates in the list of dates that are 
 * in any of the months in the list of months. Assume the list of months has 
 * no number repeated.
 *)
fun number_in_months(dates: (int*int*int) list, months: int list) =
  if null months
  then 0
  else 
    number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* Problem:
 *  Write a function dates_in_month that takes a list of dates and a month 
 *  (i.e., an int) and returns a list holding the dates from the argument 
 *  list of dates that are in the month. The returned list should contain 
 *  dates in the order they were originally given.
 *)
fun dates_in_month(dates: (int*int*int) list, month: int) =
  if null dates
  then []
  else 
    let 
      val date = hd dates
    in
      if #2 date = month
      then date::dates_in_month(tl dates, month)
      else dates_in_month(tl dates, month)
    end

(* Problem:
 * Write a function dates_in_months that takes a list of dates and 
 * a list of months (i.e., an int list) and returns a list holding 
 * the dates from the argument list of dates that are in any of 
 * the months in the list of months. Assume the list of months has 
 * no number repeated. Hint: Use your answer to the previous problem 
 * and SML’s list-append operator (@).
 *)
fun dates_in_months(dates: (int*int*int) list, months: int list) =
  if null months
  then []
  else 
    dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(* Problem:
 * Write a function get_nth that takes a list of strings and an int n 
 * and returns the nth element of the list where the head of the list 
 * is 1st. Do not worry about the case where the list has too few 
 * elements: your function may apply hd or tl to the empty list in this 
 * case, which is okay.
 *)
fun get_nth(xs: string list, n: int) =
  if n = 1
  then hd xs
  else get_nth(tl xs, n - 1)

(* Problem:
 * Write a function date_to_string that takes a date and returns a string 
 * of the form January 20, 2013 (for example). Use the operator ^ for 
 * concatenating strings and the library function Int.toString for converting 
 * an int to a string. For producing the month part, do not use a bunch of 
 * conditionals. Instead, use a list holding 12 strings and your answer to 
 * the previous problem. For consistency, put a comma following the day and 
 * use capitalized English month names: January, February, March, April,
 * May, June, July, August, September, October, November, December.
 *)
fun date_to_string(date: (int*int*int)) =
  let 
    val months = [ "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December" ]
    val year = #1 date
    val month = #2 date
    val day = #3 date
    val m = get_nth(months, month - 1)
    val d = Int.toString(day)
    val y = Int.toString(year)
  in
    m ^ " " ^ d ^ ", " ^ y
  end

(* Problem:
 * Write a function number_before_reaching_sum that takes an int called sum,
 * which you can assume is positive, and an int list, which you can assume 
 * contains all positive numbers, and returns an int. You should return an 
 * int n such that the first n elements of the list add to less than sum, 
 * but the first n + 1 elements of the list add to sum or more. Assume the 
 * entire list sums to more than the passed in value; it is okay for an 
 * exception to occur if this is not the case.
 *) 
fun number_before_reaching_sum(sum: int, xs: int list) =
  if sum > hd xs then 1 + number_before_reaching_sum(sum - hd xs, tl xs)
  else 0

(* Problem:
 *  Write a function what_month that takes a day of year (i.e., an int between 1
 *  and 365) and returns what month that day is in 
 *  (1 for January, 2 for February, etc.). Use a list holding 12 integers and your
 *  answer to the previous problem.
 *)
fun what_month(day: int) =
  number_before_reaching_sum(day, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]) + 1;

(* Problem:
 *  Write a function month_range that takes two days of the year day1 and day2
 *  and returns an int list [m1,m2,...,mn] where 
 *  m1 is the month of day1, 
 *  m2 is the month of day1+1,
 *  ..., and mn is the month of day day2. 
 *  Note the result will have length day2 - day1 + 1 or length 0 if day1>day2
 *)
fun month_range(day1: int, day2: int) =
  if day1 > day2
  then []
  else what_month(day1) :: month_range(day1 + 1, day2)

(* Problem:
 * Write a function oldest that takes a list of dates and evaluates to an
 * (int*int*int) option. It evaluates to NONE if the list has no dates and 
 * SOME d if the date d is the oldest date in the list.
 *)
fun oldest(dates: (int*int*int) list) =
  if null dates
  then NONE
  else 
    let 
      fun helper(oldest: (int*int*int), dates: (int*int*int) list) =
        if null dates
        then oldest
        else 
          let 
            val oldest = if is_older(oldest, hd dates) then hd dates else oldest
          in
            helper(oldest, tl dates)
          end
    in
      SOME (helper(hd dates, tl dates))
    end
