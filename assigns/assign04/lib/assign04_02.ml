type expr = 
    | True
    | False
    | Num of int
    | Or of expr * expr
    | Add of expr * expr
    | IfThenElse of expr * expr * expr

type ty = 
    | Int
    | Bool

let rec type_of e =
    match e with
    | True -> Some Bool
    | False -> Some Bool
    | Num _ -> Some Int
    | Or (a, b) -> 
            if (type_of a) = (type_of b) then (type_of a)
            else None
    | Add (a, b) ->
            if (type_of a = Some Int) && (type_of b = Some Int) then Some Int else None
    | IfThenElse (a, b, c) ->
            if (type_of a) = Some Bool && (type_of b = type_of c) then type_of b
            else None
