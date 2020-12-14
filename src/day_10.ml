let solve_1 (xs : int array) : int =
    let n : int = Array.length xs in
    let rec loop (i : int) (a : int) (b : int) : (int * int) =
        let j : int = i + 1 in
        if j = n then
            (a, b + 1)
        else
        if xs.(j) - xs.(i) = 3 then
            loop (i + 1) a (b + 1)
        else if xs.(j) - xs.(i) = 1 then
            loop (i + 1) (a + 1) b
        else
            loop (i + 1) a b in
    let (a, b) : (int * int) =
        let x : int = xs.(0) in
        if x = 1 then
            loop 0 1 0
        else if x = 3 then
            loop 0 0 1
        else
            loop 0 0 0 in
    a * b

let () : unit =
    let xs : int array =
        Prelude.read_file Sys.argv.(1)
        |> Prelude.split_newlines
        |> Array.map (fun x -> Prelude.str_to_int x |> Option.get) in
    Array.sort compare xs;
    solve_1 xs |> Printf.printf "%d\n"
