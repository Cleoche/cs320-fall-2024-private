let last_function_standing funcs start pred = 
    let rec make_output funcs = 
        match funcs with 
        | [] -> []
        | _::t -> start::(make_output t)
    in
    let rec in_iterate funcs outputs keep new_outs = 
        match funcs, outputs with 
        | [], [] -> (keep, new_outs)
        | h::t, x::y -> let out = h x in
                        if (pred out) then in_iterate t y keep new_outs
                        else in_iterate t y (h::keep) (out::new_outs)
        | _, _ -> assert false
    in
    let rec out_iterate funcs outputs= 
        match funcs with
        | [] -> None
        | h::t ->
                if t = [] then (Some h)
                else let (nfuncs, noutputs) = in_iterate funcs outputs [] []
                in
                out_iterate nfuncs noutputs
    in
    let outs = make_output funcs
    in
    out_iterate funcs outs
