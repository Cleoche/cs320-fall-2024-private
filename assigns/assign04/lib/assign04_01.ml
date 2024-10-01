let last_function_standing funcs start pred = 
    let rec test f input =
        let out = f input
        in
        if (not (pred out)) then 1 + test f out else 0
    in
    let rec iterate fs iter f = 
        match fs with 
        | [] -> f
        | h::t -> let out = test h start in if (iter = out) then iterate t iter None
            else if (out > iter) then iterate t out (Some h) else iterate t iter f
    in
    iterate funcs (-1) None

