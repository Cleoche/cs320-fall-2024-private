type tree = 
    | Leaf of int
    | Node of tree list

let collapse h t =
    let rec height t = 
        match t with
        | Leaf _ -> 0
        | Node cs -> 
                let rec max_depth cs = 
                    match cs with 
                    | [] -> -1
                    | c::cs -> max (height c) (max_depth cs)
                in 1 + max_depth cs
    in
    let hgt = height t 
    in
    if hgt = 1 || hgt <= h then t else
        let rec fold lst = 
            match lst with
            | [] -> []
            | (Leaf i)::t -> (Leaf i)::(fold t)
            | (Node cs)::t -> 
                    if cs = [] then (Node cs)::(fold t)
                    else (fold cs) @ (fold t)
        in
        let rec traverse h lst = 
            match lst with
            | [] -> []
            | (Leaf i)::t -> (Leaf i)::(traverse h t)
            | (Node cs)::t -> 
                    if cs = [] then (Node cs)::(traverse h t)
                    else if h = 1 then (fold cs)@(traverse h t)
                    else (Node (traverse (h - 1) cs))::(traverse h t)
        in
        match t with
        | Leaf i -> Leaf i
        | Node cs -> Node (traverse h cs)
