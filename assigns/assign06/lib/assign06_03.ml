open Utils
let rec type_of ex = 
    match ex with
    | Num _ -> Some TInt
    | Add (a, b) -> if (type_of a = Some TInt) && (type_of b = Some TInt) then Some TInt else None
    | Lt (a, b) -> if (type_of a = Some TInt) && (type_of b = Some TInt) then Some TBool else None
    | Ite (a, b, c) -> if (type_of a = Some TBool) && (type_of b = type_of c) then type_of c else None
    

