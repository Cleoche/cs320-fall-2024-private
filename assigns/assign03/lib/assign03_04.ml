let group lst = 
    let rec valid lst neg = 
        match lst with
        | [] -> true
        | 0::[] -> false
        | 0::0::_ -> false
        | 0::t -> valid t (not neg)
        | h::t -> 
                if (neg && h < 0) || ((not neg) && h > 0) then valid t neg
                else false
    in
    let rec current lst = 
        match lst with
        | [] -> []
        | 0::_ -> []
        | h::t -> h::(current t)
    in
    let rec rem lst =
        match lst with
        | [] -> []
        | 0::t -> t
        | _::t -> rem t
    in
    let rec grouping lst =
        match lst with
        | [] -> []
        | _ -> 
                let (group, rest) = (current lst, rem lst)
                in
                group::(grouping rest)
    in
    match lst with
    | [] -> None
    | 0::_ -> None
    | h::_ -> 
            if valid lst (h < 0) then Some (grouping lst)
            else None
