let solve_1 (a : int) (xs : int array) : int option =
    let n : int = Array.length xs in
    let set : (int, unit) Hashtbl.t = Hashtbl.create n in
    Array.iter (fun x -> Hashtbl.add set x ()) xs;
    let rec loop (i : int) : int option =
        if i < 0 then
            None
        else
            let b : int = xs.(i) in
            let c : int = (2020 - a) - b in
            match Hashtbl.find_opt set c with
                | Some _ -> Some (b * c)
                | None -> loop (i - 1) in
    loop (n - 1)

let solve_2 (xs : int array) : int option =
    let n : int = Array.length xs in
    let rec loop (i : int) : int option =
        if i < 0 then
            None
        else
            let a : int = xs.(i) in
            match solve_1 a xs with
                | Some bc -> Some (a * bc)
                | None -> loop (i - 1) in
    loop (n - 1)

let () : unit =
    let xs : int array =
        Prelude.read_file Sys.argv.(1)
        |> Prelude.split_newlines
        |> Array.map int_of_string in
    List.iter
        (fun f -> f xs |> Option.iter (Printf.printf "%d\n"))
        [solve_1 0; solve_2]
