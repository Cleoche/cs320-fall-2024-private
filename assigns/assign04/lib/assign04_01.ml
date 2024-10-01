let last_function_standing funcs start pred = 
    let rec test f max input =
        if max = 0 then 1 else
        let out = f input
        in
        if (not (pred out)) then 1 + test f out (max - 1) else 0
    in
    let rec iterate fs iter f max = 
        match fs with 
        | [] -> f
        | h::t -> let out = test h start max in 
            if (out - 1000 >= iter) then
                iterate (t@[h]) out (Some h) (max + 1000)
            else if (iter = out) then iterate t iter None max
            else if (out > iter) then iterate t out (Some h) max else iterate t iter f max
    in
    iterate funcs (-1) None 1000

