let add_begin_end (xs : int array) : int array =
    let n : int = Array.length xs in
    let m : int = n - 1 in
    let xs' : int array = Array.make (n + 2) 0 in
    for i = 0 to m do
        xs'.(i + 1) <- xs.(i)
    done;
    xs'.(n + 1) <- xs.(m) + 3;
    xs'

let solve_1 (xs : int array) : int =
    let n : int = (Array.length xs) in
    let rec loop (i : int) (a : int) (b : int) : (int * int) =
        let j : int = i + 1 in
        if j = n then
            (a, b)
        else if xs.(j) - xs.(i) = 3 then
            loop (i + 1) a (b + 1)
        else if xs.(j) - xs.(i) = 1 then
            loop (i + 1) (a + 1) b
        else
            loop (i + 1) a b in
    let (a, b) : (int * int) = loop 0 0 0 in
    a * b

let solve_2 (xs : int array) : int =
    let n : int = Array.length xs in
    let m : int = n - 1 in
    let state : int array = Array.make n 0 in
    state.(0) <- 1;
    for i = 0 to m do
        let x : int = xs.(i) in
        let s : int = state.(i) in
        for j = (i + 1) to (i + 3) do
            if (j < n) && ((xs.(j) - x) <= 3) then (
                state.(j) <- state.(j) + s
            );
        done;
    done;
    state.(m)

let () : unit =
    let xs : int array =
        Prelude.read_file Sys.argv.(1)
        |> Prelude.split_newlines
        |> Array.map (fun x -> Prelude.str_to_int x |> Option.get) in
    Array.sort compare xs;
    let xs : int array = add_begin_end xs in
    List.iter (fun f -> f xs |> Printf.printf "%d\n") [solve_1; solve_2]
