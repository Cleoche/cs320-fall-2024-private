type ident = string

type expr' = 
    | True
    | False
    | Num of int
    | Var of ident
    | Let of ident * expr' * expr'
    | Add of expr' * expr'
    | Or of expr' * expr'
    | IfThenElse of expr' * expr' * expr'

type ty' =
    | Int
    | Bool

type context = (ident * ty') list

let rec type_of' cx e =
    let rec iterate lst key = 
        match lst with
        | [] -> None
        | (a, b)::t -> if a = key then Some b else iterate t key
    in
    match e with
    | True -> Some Bool
    | False -> Some Bool
    | Num _ -> Some Int
    | Var a -> iterate cx a
    | Let (a, b, c) -> (match type_of' cx b with 
                        | Some y -> type_of' ((a, y)::cx) c 
                        | _ -> None)
    | Add (a, b) -> if (type_of' cx a = Some Int) && (type_of' cx b = Some Int) then Some Int else None
    | Or (a, b) -> if (type_of' cx a = Some Bool) && (type_of' cx b = Some Bool) then Some Bool else None
    | IfThenElse (a, b, c) -> if (type_of' cx a = Some Bool) && (type_of' cx b = type_of' cx c) then type_of' cx b 
    else None
