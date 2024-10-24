open Utils
let parse lst = 
    let rec aux lst stack = 
        match lst with
        | [] -> stack
        | current::rest -> 
                match current with
                | TNum x -> aux rest ((Num x)::stack)
                | TAdd -> (match stack with
                            | a::b::c -> aux rest ((Add (b, a))::c)
                            | _ -> [])
                | TLt -> (match stack with
                            | a::b::c -> aux rest ((Lt (b, a))::c)
                            | _ -> [])
                | TIte -> (match stack with
                            | a::b::c::d -> aux rest ((Ite (c, b, a))::d)
                            | _ -> [])
    in
    match aux lst [] with
    | [a] -> Some a
    | _ -> None
