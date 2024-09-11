let to_string n = 
    let rec f k i out = 
        if k < 2 then out ^ "]"
        else
            let num = Assign01_03.nth k i in
            let newk = (k / (Assign01_01.pow (Assign01_02.nth_prime i) num)) in
            if i = 0 then f newk (i + 1) (out ^ (string_of_int num))
            else f newk (i + 1) (out ^ "; " ^ (string_of_int num))
    in f n 0 "["
