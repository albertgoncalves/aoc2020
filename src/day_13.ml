let parse (xs : string array) : (int * (int * int) list) =
    let s : string = xs.(1) in
    let m : int = String.length s in
    let q : (int * int) Queue.t = Queue.create () in
    let f (i : int) (j : int) (l : int) : unit =
        match String.sub s i (j - i) |> Prelude.str_to_int with
            | Some x -> Queue.add (l, x) q
            | None -> () in
    let rec loop (i : int) (j : int) (l : int) : unit =
        if j = m then (
            if i <> j then
                f i j l
        ) else if s.[j] = ',' then (
            f i j l;
            loop (j + 1) (j + 2) (l + 1)
        ) else
            loop i (j + 1) l in
    loop 0 1 0;
    (Prelude.str_to_int xs.(0) |> Option.get, Queue.to_seq q |> List.of_seq)

let solve_1 (n : int) (xs : (int * int) list) : int =
    let f ((x, a) : (int * int)) ((x', b) : (int * int)) : (int * int) =
        if a < b then
            (x, a)
        else
            (x', b) in
    let (x, m) : (int * int) =
        List.map (fun (_, v) -> (v, (v - (n mod v)))) xs
        |> List.fold_left f (0, max_int) in
    List.iter (fun (i, v) -> Printf.printf "i: %3d, v: %3d\n" i v) xs;
    x * m

let solve_2 (xs : (int * int) list) : int =
    let rec loop (n : int) (m : int) (x' : int) : (int * int) list -> int =
        function
            | [] -> n
            | (i, v)::xs as xs' ->
                if ((n + i) mod v) = 0 then
                    let m : int =
                        if x' < v then
                            m * v
                        else
                            m in
                    loop n m v xs
                else
                    loop (n + m) m x' xs' in
    loop 1 1 1 (List.sort (fun (_, a) (_, b) -> compare a b) xs)

let () : unit =
    let (n, xs) : (int * (int * int) list) =
        Prelude.read_file Sys.argv.(1) |> Prelude.split_newlines |> parse in
    List.iter (Printf.printf "%d\n") [solve_1 n xs; solve_2 xs]
