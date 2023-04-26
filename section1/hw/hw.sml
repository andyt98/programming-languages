(* a date is an SML value of type int * int * int (year month day) *)

fun is_older(d1: int * int * int, d2: int * int * int) =
        (#1 d1 < #1 d2) orelse
        (#1 d1 = #1 d2 andalso #2 d1 < #2 d2) orelse
        (#1 d1 = #1 d2 andalso #2 d1 = #2 d2 andalso #3 d1 < #3 d2)

fun number_in_month(ds: (int * int * int) list, m: int) = 
        if null ds
        then 0
        else if #2 (hd ds) = m
            then number_in_month(tl ds, m) + 1
            else number_in_month(tl ds, m)

(* Assume the list of months has no number repeated. *)
fun number_in_months(ds: (int * int * int) list, ms: int list) = 
        if null ms 
        then 0
        else number_in_month(ds, hd ms) + number_in_months(ds, tl ms)

fun dates_in_month(ds: (int * int * int) list, m: int) =
        if null ds
        then []
        else 
            if #2 (hd ds) = m
            then (hd ds) :: dates_in_month(tl ds, m)
            else dates_in_month(tl ds, m)


(* Assume the list of months has no number repeated. *)
fun dates_in_months(ds: (int * int * int) list, ms: int list) =
        if null ms
        then []
        else dates_in_month(ds, hd ms) @ dates_in_months(ds, tl ms)

fun get_nth(ss: string list, n: int) = 
        if null ss
        then ""
        else if n = 1 
            then hd ss
            else get_nth(tl ss, n - 1)

fun date_to_string(d: (int * int * int)) =
        let 
            val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
        in
            get_nth(months, #2 d) ^ " " ^  Int.toString(#3 d) ^ ", " ^ Int.toString(#1 d)
        end


fun number_before_reaching_sum(sum: int, ns: int list) =
        let
            fun helper(n: int, ns: int list) =
                    if null ns orelse n + hd ns >= sum then 0
                    else 1 + helper(n + hd ns, tl ns)
        in
            helper(0, ns)
        end

fun what_month(d: int) =
        let 
            val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        in
            number_before_reaching_sum(d, days_in_months) + 1
        end


fun month_range(d1: int, d2: int) = 
        if d1 > d2
        then []
        else what_month(d1) :: (month_range(d1 + 1, d2))


fun oldest(ds: (int * int * int) list) =
        if null ds
        then NONE
        else 
            let
                fun oldest_nonempty(ds: (int * int * int) list) =
                        if null (tl ds)
                        then hd ds
                        else let val oldest_so_far = oldest_nonempty(tl ds)
                            in
                                if is_older(hd ds, oldest_so_far)
                                then hd ds
                                else oldest_so_far
                            end
            in
                SOME (oldest_nonempty ds)
            end

fun contains(n, ns) = 
        if null ns
        then false
        else if hd ns = n
            then true
            else contains(n, tl ns)

fun remove_duplicates(ns: int list) = 
        if null ns
        then []
        else
            if contains(hd ns, tl ns)
            then  remove_duplicates(tl ns)
            else (hd ns) :: remove_duplicates(tl ns) 

fun number_in_months_challenge(ds: (int * int * int) list, ms: int list)  = 
        number_in_months(ds, remove_duplicates(ms))

fun dates_in_months_challenge(ds: (int * int * int) list, ms: int list) = 
        dates_in_months(ds, remove_duplicates(ms))

fun reasonable_date(d: int * int * int) =
        let
            val days_in_months_non_leap = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
            val days_in_months_leap = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

            fun get_nth(ns: int list, n: int) = 
                    if null ns
                    then 0
                    else if n = 1 
                        then hd ns
                        else get_nth(tl ns, n - 1)

            fun is_leap_year(year: int) =
                    (year mod 400 = 0) orelse (year mod 4 = 0 andalso year mod 100 <> 0)

            fun is_valid_day(d: int * int * int) = 
                    (is_leap_year(#1 d) andalso #3 d <= get_nth(days_in_months_leap, #2 d))
                    orelse
                    ((not (is_leap_year (#1 d))) andalso #3 d <= get_nth(days_in_months_non_leap, #2 d))
        in
            (#1 d > 0) andalso (#2 d >= 1 andalso #2 d <= 12) andalso is_valid_day(d)  
        end



