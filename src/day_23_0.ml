let split_by (n : int) (xs : 'a list) : ('a list * 'a list) =
    let rec loop
            (n : int)
            (ls : 'a list)
            (rs : 'a list) : ('a list * 'a list) =
        if n <= 0 then
            (ls, rs)
        else
            match (ls, rs) with
                | (_, []) -> (ls, rs)
                | (ls, r :: rs) -> (loop [@tailcall]) (n - 1) (r :: ls) rs in
    let (ls, rs) : ('a list * 'a list) = loop n [] xs in
    (List.rev ls, rs)

let insert_at (f : 'a -> bool) (insert : 'a list) : 'a list -> 'a list =
    let rec loop (ls : 'a list) : 'a list -> 'a list = function
        | [] -> List.append ls insert
        | x :: rs ->
            if f x then
                List.rev_append ls (List.append (x :: insert) rs)
            else
                (loop [@tailcall]) (x :: ls) rs in
    loop []

let cycle (n : int) (xs : 'a list) :'a list =
    let (ls, rs) : ('a list * 'a list) = split_by n xs in
    List.append rs ls

let within (m : int) (x : int) : int =
    (m + x) mod m

let find_destination (n : int) (m : int) (xs : int list) =
    let rec loop (n : int) : int list -> int = function
        | [] -> loop (within m (n - 1)) xs
        | x :: xs ->
            if (x mod m) = n then
                x
            else
                (loop [@tailcall]) n xs in
    loop (within m n) xs

let split_at (f : 'a -> bool) (xs : 'a list) : ('a list * 'a list) =
    let rec loop
            (ls : 'a list)
            (rs : 'a list) : 'a list -> ('a list * 'a list) = function
        | [] -> (ls, [])
        | x :: xs ->
            if f x then
                (ls, xs)
            else
                (loop [@tailcall]) (x :: ls) rs xs in
    let (ls, rs) : ('a list * 'a list) = loop [] [] xs in
    (List.rev ls, rs)

let rec iterate (f : 'a -> 'a) (n : int) (x : 'a) : 'a =
    if n <= 0 then
        x
    else
        (iterate [@tailcall]) f (n - 1) (f x)

let run (n : int) : int list -> int list = function
    | [] ->
        (
            Printf.eprintf "run\n";
            exit 1
        )
    | x :: xs ->
        let (ls, rs) : (int list * int list) = split_by 3 xs in
        let xs : int list =
            insert_at ((=) (find_destination (within n (x - 1)) n rs)) ls rs
            |> List.rev in
        List.rev_append xs [x]

let range (a : int) (b : int) : int list =
    let rec loop (xs : int list) (a : int) : int list =
        if a = b then
            List.rev (a :: xs)
        else
            (loop [@tailcall]) (a :: xs) (a + 1) in
    loop [] a

let rec take (n : int) : 'a list -> 'a list = function
    | [] -> []
    | x :: xs ->
        if n = 0 then
            []
        else
            x :: (take (n - 1) xs)

let solve_1 (xs : int list) : int list =
    let (ls, rs) : (int list * int list) =
        iterate (run 9) 100 xs |> split_at ((=) 1) in
    List.append rs ls

let show (xs : int list) : unit =
    List.iter (Printf.printf "%d") xs;
    Printf.printf "\n"

let () : unit =
    List.iter
        (fun xs -> solve_1 xs |> show)
        [
            [3; 8; 9; 1; 2; 5; 4; 6; 7];
            [8; 7; 2; 4; 9; 5; 1; 3; 6];
        ]
