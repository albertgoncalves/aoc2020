let parse (xs : string array) : (int Queue.t * int Queue.t) =
    let n : int = Array.length xs in
    let rec loop (xs' : int Queue.t) (i : int) : int =
        if i = n then
            i
        else
            match Prelude.str_to_int xs.(i) with
                | Some x ->
                    (
                        Queue.add x xs';
                        loop xs' (i + 1)
                    )
                | None -> i + 1 in
    let ls : int Queue.t = Queue.create () in
    let rs : int Queue.t = Queue.create () in
    loop ls 1 |> loop rs |> ignore;
    (ls, rs)

let rec run_1 (ls : int Queue.t) (rs : int Queue.t) : int Queue.t =
    if Queue.is_empty rs then
        ls
    else if Queue.is_empty ls then
        rs
    else
        let l : int = Queue.take ls in
        let r : int = Queue.take rs in
        if r < l then (
            Queue.add l ls;
            Queue.add r ls;
            run_1 ls rs
        ) else (
            Queue.add r rs;
            Queue.add l rs;
            run_1 ls rs
        )

let drain (a : 'a Queue.t) (n : int) : 'a Queue.t =
    let b : 'a Queue.t = Queue.create () in
    for _ = 1 to min n (Queue.length a) do
        Queue.add (Queue.take a) b
    done;
    b

let rec run_2
        (memo : ((int Queue.t * int Queue.t), unit) Hashtbl.t)
        (ls : int Queue.t)
        (rs : int Queue.t) : (bool * int Queue.t) =
    if (Queue.is_empty rs) || (Hashtbl.mem memo (ls, rs)) then
        (true, ls)
    else if Queue.is_empty ls then
        (false, rs)
    else (
        Hashtbl.add memo (Queue.copy ls, Queue.copy rs) ();
        let l : int = Queue.take ls in
        let r : int = Queue.take rs in
        if (l <= (Queue.length ls)) && (r <= (Queue.length rs)) then (
            let (left, _) : (bool * int Queue.t) =
                run_2
                    (Hashtbl.create 32)
                    (drain (Queue.copy ls) l)
                    (drain (Queue.copy rs) r) in
            if left then (
                Queue.add l ls;
                Queue.add r ls;
                run_2 memo ls rs
            ) else (
                Queue.add r rs;
                Queue.add l rs;
                run_2 memo ls rs
            )
        ) else if r < l then (
            Queue.add l ls;
            Queue.add r ls;
            run_2 memo ls rs
        ) else (
            Queue.add r rs;
            Queue.add l rs;
            run_2 memo ls rs
        )
    )

let score (xs : int Queue.t) : int =
    Queue.to_seq xs
    |> List.of_seq
    |> List.rev
    |> List.mapi (fun i x -> (i + 1) * x)
    |> List.fold_left (+) 0

let () : unit =
    let (ls, rs) : (int Queue.t * int Queue.t) =
        Prelude.read_file Sys.argv.(1) |> Prelude.split_newlines |> parse in
    run_1 (Queue.copy ls) (Queue.copy rs) |> score |> Printf.printf "%d\n%!";
    run_2 (Hashtbl.create 32) ls rs |> snd |> score |> Printf.printf "%d\n"
