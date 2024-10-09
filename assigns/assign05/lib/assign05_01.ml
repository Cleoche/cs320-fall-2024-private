type 'a test = 
    | TestCase of 'a
    | TestList of 'a test list

let rec fold_left op base test =
    match test with
    | TestCase a -> op base a
    | TestList a -> (match a with
                    | [] -> base
                    | h::t -> fold_left op (fold_left op base h) (TestList t))
