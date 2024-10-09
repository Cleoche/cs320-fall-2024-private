type set_info = 
    { ind : int -> bool;
    mn : int;
    mx : int
    }

module ListSet = struct
    type t = int list
    let rec mem x s = 
        match s with
        | [] -> false
        | h::t -> 
                if x = h then true
                else mem x t
    let empty = []
    let singleton n = [n]
    let rec card s = 
        match s with
        | [] -> 0
        | _::t -> 1 + (card t)
    let rec union s1 s2 = 
        match s1, s2 with
        | a, [] -> a
        | [], a -> a
        | x::s11, y::s22 -> 
                if x = y then x::(union s11 s22)
                else if x > y then y::(union s1 s22)
                else x::(union s11 s2)

end

module FuncSet = struct
    type t = set_info
    let mem x s = s.ind x
    let empty = {ind = (fun _ -> false); mn = 0; mx = -1}
    let singleton n = {ind = (fun x -> x = n); mn = n; mx = n}
    let card s = 
        let rec checker n = 
            if n > s.mx then 0
            else if s.ind n then 1 + checker (n + 1)
            else checker (n + 1)
        in
        checker s.mn
    let union s1 s2 = 
        if s1.mn > s1.mx then s2
        else if s2.mn > s2.mx then s1
        else
            let absmin = if s1.mn < s2.mn then s1.mn else s2.mn
            in
            let absmax = if s1.mx > s2.mx then s1.mx else s2.mx
            in
            let rec checker n = 
               if n > absmax then []
               else if (n >= s1.mn && n <= s1.mx && s1.ind n) || (n >= s2.mn && n <= s2.mx && s2.ind n) 
               then n::(checker (n + 1))
               else checker (n + 1)
            in
            let lst = checker absmin
            in
            let ind_func n = 
                let rec iterate lst = 
                    match lst with 
                    | [] -> false
                    | h::t -> 
                            if h = n then true
                            else iterate t
                in
                iterate lst
            in
            {ind = ind_func; mn = absmin; mx = absmax}

end
