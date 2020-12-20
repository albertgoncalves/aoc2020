let run (xs : int array) (n : int) : int =
    let mem : (int, int) Hashtbl.t = Hashtbl.create n in
    let m : int = Array.length xs in
    Array.iteri
        (fun i x -> Hashtbl.replace mem x (i + 1))
        (Array.sub xs 0 (m - 1));
    let rec loop (i : int) (x : int) : int =
        if i = n then
            x
        else
            match Hashtbl.find_opt mem x with
                | None ->
                    (
                        Hashtbl.add mem x i;
                        loop (i + 1) 0
                    )
                | Some j ->
                    (
                        Hashtbl.replace mem x i;
                        loop (i + 1) (i - j)
                    ) in
    loop m xs.(m - 1)

let solve_all (n : int) : int array array -> unit =
    Array.iter (fun xs -> run xs n |> Printf.printf "%d\n%!")

let xs : int array array =
    [|
        [|0; 3; 6|];
        [|1; 3; 2|];
        [|2; 1; 3|];
        [|1; 2; 3|];
        [|2; 3; 1|];
        [|3; 2; 1|];
        [|3; 1; 2|];
        [|16; 1; 0; 18; 12; 14; 19|];
    |]

let () : unit =
    Array.iter (fun n -> solve_all n xs) [|2020; 30000000|]
