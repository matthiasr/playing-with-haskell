sieve :: Int -> [Int]
sieve 2 = [2]
sieve 3 = [3,2]
sieve n | n > 3 =
    let ps = sieve (n-1) in
        if all (\x -> not ((mod n x) == 0)) ps
        then n:ps
        else ps

