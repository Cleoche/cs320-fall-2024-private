type dir =
    | North
    | South
    | East
    | West

type path = dir list

let dist lst = 
    let rec separate lst n e s w = 
        match lst with 
        | [] -> (n,e,s,w)
        | h::t -> 
                match h with
                | North -> separate t (n+1) e s w
                | South -> separate t n e (s+1) w
                | East -> separate t n (e+1) s w
                | West -> separate t n e s (w+1)
    in
    let (n, e, s, w) = separate lst 0 0 0 0 in
    let (l,w) = ((n-s), (e-w)) in
    sqrt (float_of_int ((l * l) + (w*w)))
