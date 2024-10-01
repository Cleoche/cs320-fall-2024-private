open Assign04_02

type value =
    | VNum of int
    | VBool of bool

let rec eval e = 
    let add a b = 
        match a with
        | VNum x -> (match b with 
            | VNum y -> VNum (x+y)
            | _ -> assert false)
        | _ -> assert false
    in
    let vor a b = 
        match a with
        | VBool x -> (match b with
            | VBool y -> VBool (x||y)
            | _ -> assert false)
        | _ -> assert false
    in
    match e with
    | True -> VBool true
    | False -> VBool false
    | Num a -> VNum a
    | Or (a, b) -> vor (eval a) (eval b)
    | Add (a, b) -> add (eval a) (eval b)
    | IfThenElse (a, b, c) -> if (eval a = VBool true) then (eval b) else (eval c)
