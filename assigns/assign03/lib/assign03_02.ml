let gen_fib lst index = 
    let rec length lst =
        match lst with
        | [] -> 0
        | _::t -> 1 + length t
    in
    let rec find lst index = 
        match lst, index with
        | h::_, 0 -> h
        | _::t, i -> find t (i-1)
        | _, _ -> 0
    in
    let rec add lst =
        match lst with
        | [] -> 0
        | h::t -> h + (add t)
    in
    let len = length lst in
    if len > index then find lst index else
    let rec fib lst index =
        if index = 0 then add lst
        else match lst with
        | [] -> 0
        | _::t -> 
                fib (t@[(add lst)]) (index - 1)
    in
    fib lst (index - len)
