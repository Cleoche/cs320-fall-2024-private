let mk_unique_keys lst = 
    let rec rem lst key =
        match lst with
        | [] -> []
        | (h, k)::t -> 
                if h = key then (rem t key) else (h, k)::(rem t key)
    in
    let rec add lst key = 
        match lst with
        | [] -> 0
        | (h, k)::t -> 
                if h = key then k + (add t key) else add t key
    in
    let rec prtn lst = 
        match lst with
        | [] -> []
        | (h, _)::_ -> 
            let rest = rem lst h in
        let v = add lst h in
        (h, v)::prtn rest in

    prtn lst

