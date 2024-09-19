type matrix = {
    entries : float list list;
    rows : int;
    cols : int;
}


let mk_matrix lst (r,c) = 
    let rec list_maker lst n = 
        match lst with
        | [] -> []
        | _ -> 
            let rec row_maker lst n = 
                match n, lst with 
                | 0, _ | _, [] -> [] 
                | _, h::t -> h::row_maker t (n-1)
            in
            let rec rem lst n = 
                match n, lst with
                | 0, _ | _, [] -> lst
                | n, _::t -> rem t (n-1)
            in 
            let current = row_maker lst n in
            let rest = rem lst n in
            current :: list_maker rest n
    in {entries = list_maker lst c; rows = r; cols = c}
