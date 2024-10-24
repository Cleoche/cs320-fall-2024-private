open Utils
let lex s = 
    let lst = split s
    in
    let rec aux s out=
        match s with
        | [] -> out
        | current::rest -> 
            match tok_of_string_opt current with
            | Some k -> aux rest (out@[k])
            | None -> []
    in
    match aux lst [] with
    | [] -> None
    | k -> Some k
