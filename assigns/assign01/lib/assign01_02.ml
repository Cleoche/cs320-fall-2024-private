let is_prime k =
    if k < 2 then
        false
    else
        let rec divisors d = 
            if d * d > k then true
            else if k mod d = 0 then false
            else divisors (d + 1)
        in divisors 2


let nth_prime n = 
    let rec prime_counter count current = 
        if count = n then current
        else
            if is_prime (current + 1) then
                prime_counter (count + 1) (current + 1)
            else
                prime_counter count (current + 1)
    in prime_counter 0 2
