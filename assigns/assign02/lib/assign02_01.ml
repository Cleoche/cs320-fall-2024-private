type piece = 
    | X
    | O

type pos = 
    | Piece of piece
    | Blank

type board = ( pos * pos * pos ) * ( pos * pos * pos ) * ( pos * pos * pos )

type row_index = 
    | Top
    | Middle
    | Bottom

type col_index = 
    | Left
    | Middle
    | Right

type pos_index = row_index * col_index

let get_pos board index = 
    let ((a1, b1, c1), (a2, b2, c2), (a3, b3, c3)) = board in
    match index with
    | ( Top, Left ) -> a1
    | ( Top, Middle ) -> b1
    | ( Top, Right ) -> c1
    | ( Middle, Left ) -> a2
    | ( Middle, Middle ) -> b2
    | ( Middle, Right) -> c2
    | ( Bottom, Left ) -> a3
    | ( Bottom, Middle ) -> b3
    | ( Bottom, Right ) -> c3

let winner board = 
    let ((a1, b1, c1), (a2, b2, c2), (a3, b3, c3)) = board in
    let patterns = [(a1, b1, c1);(a2, b2, c2);(a3,b3,c3);(a1,a2,a3);(b1,b2,b3);(c1,c2,c3);(a1,b2,c3);(a3,b2,c1)] in
    let rec checker tuples = 
        match tuples with
        | [] -> false
        | (a,b,c) :: rest -> 
                if (a = Piece X && b = Piece X && c = Piece X) || (a = Piece O && b = Piece O && c = Piece O) then true
                else checker rest
    in checker patterns
