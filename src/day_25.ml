let next (n : int) (x : int) : int =
    (n * x) mod 20201227

let rec search (f : 'a -> 'a) (x : int) (m : int) (n : int) : int =
    if x = m then
        n
    else
        search f (f x) m (n + 1)

let rec iterate (f : 'a -> 'a) (x : 'a) (n : int) : 'a =
    if n <= 0 then
        x
    else
        iterate f (f x) (n - 1)

let solve (a : int) (b : int) : int =
    let n : int = search (next 7) 1 a 0 in
    iterate (next b) 1 n

let () : unit =
    Array.iter
        (fun (a, b) -> solve a b |> Printf.printf "%d\n")
        [|
            (5764801, 17807724);
            (1614360, 7734663);
        |]
