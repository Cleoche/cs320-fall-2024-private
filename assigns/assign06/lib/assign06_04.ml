open Utils
let rec eval ex =
    match ex with
    | Num a -> VNum a
    | Add (a, b) -> (match eval a, eval b with
                    | VNum x, VNum y -> VNum (x + y)
                    | _, _ -> assert false)
    | Lt (a, b) -> (match eval a, eval b with
                    | VNum x, VNum y -> VBool (x < y)
                    | _, _ -> assert false)
    | Ite (a, b, c) -> if (eval a = VBool true) then eval b else eval c
