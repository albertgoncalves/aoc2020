type t = {
    i : int;
    v : int;
}

let parse (xs : string array) : (int * t list) =
    let n : int = Prelude.str_to_int xs.(0) |> Option.get in
    let xs' : t Queue.t = Queue.create () in
    let s : string = xs.(1) in
    let m : int = String.length s in
    let rec loop (i : int) (j : int) (l : int) : unit =
        if j = m then
            ()
        else if s.[j] = ',' then (
            (
                match String.sub s i (j - i) |> Prelude.str_to_int with
                    | Some x ->
                        let x : t = {
                            i = l;
                            v = x;
                        } in
                        Queue.add x xs'
                    | None -> ()
            );
            loop (j + 1) (j + 2) (l + 1)
        ) else
            loop i (j + 1) l in
    loop 0 1 0;
    (n, Queue.to_seq xs' |> List.of_seq)

let solve_1 (n : int) (xs : t list) : int =
    let f ((x, a) : (int * int)) ((x', b) : (int * int)) : (int * int) =
        if a < b then
            (x, a)
        else
            (x', b) in
    let (x, m) : (int * int) =
        List.map (fun x -> (x.v, (x.v - (n mod x.v)))) xs
        |> List.fold_left f (0, max_int) in
    x * m

let () : unit =
    let (n, xs) : (int * t list) =
        Prelude.read_file Sys.argv.(1)
        |> Prelude.split_newlines
        |> parse in
    solve_1 n xs |> Printf.printf "%d\n"
