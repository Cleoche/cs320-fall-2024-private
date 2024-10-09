type ident = string

type ty = 
    | Unit
    | Arr of ty * ty

type expr = 
    | Var of ident
    | Fun of ident * ty * expr
    | App of expr * expr

type ctxt = (ident * ty) list

let rec check_context gamma var =
    match gamma with
    | [] -> None
    | h::k -> 
            let (v, t) = h
            in
            if v = var then Some t
            else check_context k var;;

let rec type_of gamma e = 
    match e with
    | Var x -> check_context gamma x
    | Fun (x, t, ex) -> 
            (match type_of ((x, t)::gamma) ex with 
            | None -> None
            | Some a -> Some (Arr (t, a)))
    | App (e1, e2) -> 
            match type_of gamma e1 with
            | None -> None
            | Some Unit -> None
            | Some Arr (a, b) -> 
                        if (Some a) = (type_of gamma e2) then Some b
                        else None
