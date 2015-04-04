

#load "logic.fs"
open logic.core.Logic


// Examples
let example1 q = 
    logic {
        let! y = fresh
        do! y == Int 42
        do! q == y
    }

run 1 example1

let example2 q = 
    logic {
        let! x = fresh
        let! y = fresh
        do! x == Int 42
        do! y == Int 24
        do! q == x
        do! q == y
    }

run 1 example2

let example3 q = 
    logic {
        let! x = fresh
        let! y = fresh
        do! x == Int 42
        do! y == Int 24
        do! conde [q == x;
                   q == y]
    }

run 2 example3

let rec fives x = 
    logic {
        do! conde [x == Int 5;
                   logic { return! fives x }]
    }

run 9 fives

let rec sixes x = 
    logic {
        do! conde [x == Int 6;
                   logic { return! sixes x }]
    }

run 9 (fun q -> conde [fives q; sixes q])

let rec peano n =
    logic {
        do! conde [ Str "z" == n;
                    logic {
                        let! n' = fresh
                        do! Pair (Str "s", n') == n
                        return! peano n'
                    } ]
    }

run 3 (fun q -> peano q)


let rec appendo l s out =
    logic {
        do! conde [ logic { do! l == Empty
                            do! s == out };
                    logic {
                        let! a, d, res = fresh3 
                        do! Pair (a, d) == l
                        do! Pair (a, res) == out
                        return! appendo d s res
                    }]
    }

run 2 (fun x -> appendo x (Pair (Int 3, Empty)) (Pair (Int 1, Pair (Int 2, Pair (Int 3, Empty)))))
