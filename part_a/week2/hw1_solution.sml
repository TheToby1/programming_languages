fun is_older(a : int*int*int, b : int*int*int) = 
    #1 a < #1 b orelse (#1 a = #1 b andalso (#2 a < #2 b orelse (#2 a = #2 b andalso #3 a < #3 b)))

fun number_in_month(dates : (int*int*int) list, month : int) =
    let 
        fun count_second_equal(dates : (int*int*int) list) = 
            if null dates
            then 0
            else if #2 (hd dates) = month
                then 1 + count_second_equal(tl dates)
                else count_second_equal(tl dates)
    in 
        count_second_equal(dates)
    end

fun number_in_months(dates : (int*int*int) list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates : (int*int*int) list, month : int) = 
    let 
        fun get_second_equal(dates : (int*int*int) list) = 
            if null dates
            then []
            else if #2 (hd dates) = month
                then hd dates :: get_second_equal(tl dates)
                else get_second_equal(tl dates)
    in 
        get_second_equal(dates)
    end

fun dates_in_months(dates : (int*int*int) list, months : int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(*I made this generic so that I could use it on q13*)
fun get_nth_safe(xs : 'Z list, nth : int) =
    if null xs
    then NONE
    else
        if nth = 1
        then SOME (hd xs)
        else get_nth_safe(tl xs, nth - 1)

(*Should probably throw exception on out of bounds, but haven't covered*)
fun get_nth(xs : string list, nth : int) =
    let
        val ans = get_nth_safe(xs, nth)
    in
        if isSome ans
        then valOf ans
        else "Out of Bounds"
    end

fun date_to_string(date : int*int*int) = 
    let
        val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
        get_nth(months, #2 date) ^ " " ^  Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

fun number_before_reaching_sum(sum : int, xs : int list) =
    if null xs
    then 0
    else
        let
            val calc = sum - (hd xs)
        in
            if calc <= 0
            then 0
            else 1 + number_before_reaching_sum(calc, tl xs)
        end

fun what_month(day : int ) =
    (*I could move days_in_months out as I use it later on as well, but I'm leaving it as I want each function to be self contained*)
    let
        
        val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum(day, days_in_months) + 1
    end

(* fun month_range(day1 : int, day2 : int) = 
    let
        val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

        (*Could even simplify this to leave out y, leave to keep generic*)
        fun count(x: int, y : int) =
            if x = y
            then [y]
            else x :: count(x + 1, y)

        fun what_months(days : int list) = 
            if null days
            then []
            else what_month(hd days) :: what_months(tl days)
    in
        what_months(count(day1, day2))
    end *)

fun month_range(day1 : int, day2 : int) = 
    let
        val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        if day1 = day2
        then [what_month(day2)]
        else what_month(day1) :: month_range(day1 + 1, day2)
    end

fun oldest(dates : (int*int*int) list) =
    if null dates
    then NONE
    else
        let
            val oldest_tail = oldest(tl dates)
        in
            if isSome oldest_tail andalso is_older((valOf oldest_tail), hd dates)
            then oldest_tail
            else SOME (hd dates)
        end

fun remove_duplicates(xs : int list) =
    let
        fun contains(xs : int list, x : int) = 
            not(null xs) andalso (x = (hd xs) orelse contains(tl xs, x))
    in 
        if null xs
        then []
        else
            if contains(tl xs, hd xs)
            then remove_duplicates(tl xs)
            else hd xs :: remove_duplicates(tl xs)
    end

fun number_in_months_challenge(dates : (int*int*int) list, months : int list) =
    let
        val unique_months = remove_duplicates(months)
    in
        number_in_months(dates, unique_months)
    end

fun reasonable_date(date : int*int*int) =
    let
        fun is_leap_year(year : int) = 
            (year mod 400) = 0 orelse ((year mod 4) = 0 andalso (year mod 100) <> 0)
        (*feels like ternary*)
        val days_in_months = [31, if is_leap_year(#1 date) then 29 else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        #1 date > 0 andalso #2 date > 0 andalso #3 date > 0 andalso #2 date <= 12 andalso #3 date <= (valOf (get_nth_safe(days_in_months, #2 date)))
    end
