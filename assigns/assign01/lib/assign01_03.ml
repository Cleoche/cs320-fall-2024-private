let nth s i = 
    let divisor = Assign01_02.nth_prime i in
    let rec calc div count num = 
        if num mod div = 0 then
            calc div (count + 1) (num / div)
        else count
    in calc divisor 0 s
