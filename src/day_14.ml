type t =
    | Mask of (int * int) list
    | Mem of (int * int)

let tokenize (s : string) : string array =
    let xs : string Queue.t = Queue.create () in
    let f (i : int) (j : int) : unit =
        if i <> j then
            Queue.add (String.sub s i (j - i)) xs in
    let n : int = String.length s in
    let rec loop (i : int) (j : int) : unit =
        if j = n then
            f i j
        else
            let x : char = s.[j] in
            if not ((Prelude.is_alpha x) || (Prelude.is_digit x)) then (
                f i j;
                loop (j + 1) (j + 1)
            ) else
                loop i (j + 1) in
    loop 0 0;
    Queue.to_seq xs |> Array.of_seq

let parse (xs : string array) : t =
    match xs.(0) with
        | "mask" ->
            let s : string = xs.(1) in
            let n : int = String.length s in
            let rec loop (i : int) : (int * int) list =
                if i = n then
                    []
                else
                    let x : char = s.[i] in
                    if x = 'X' then
                        loop (i + 1)
                    else
                        let x : (int * int) =
                            ((n - 1) - i, (Char.code x) - (Char.code '0')) in
                        x :: loop (i + 1) in
            Mask (loop 0)
        | "mem" ->
            let f (i : int) : int =
                Prelude.str_to_int xs.(i) |> Option.get in
            Mem (f 1, f 2)
        | _ ->
            (
                Printf.eprintf "parse\n";
                exit 1
            )

let get_max_address : t array -> int =
    let f (a : int) : t -> int = function
        | Mask _ -> a
        | Mem (b, _) ->
            if a < b then
                b
            else
                a in
    Array.fold_left f 0

let apply_mask (x : int) : (int * int) -> int = function
    | (i, 1) -> x lor (1 lsl i)
    | (i, 0) -> x land (lnot (1 lsl i))
    | _ ->
        (
            Printf.eprintf "apply_mask\n";
            exit 1
        )

let solve_1 (xs : t array) (n : int) : int =
    let mem : int array = Array.make n 0 in
    let m : int = Array.length xs in
    let rec loop (xs' : (int * int) list) (i : int) : unit =
        if i = m then
            ()
        else
            match xs.(i) with
                | Mask xs' -> loop xs' (i + 1)
                | Mem (j, v) ->
                    (
                        mem.(j) <- List.fold_left apply_mask v xs';
                        loop xs' (i + 1)
                    ) in
    (
        match xs.(0) with
            | Mask xs' -> loop xs' 1
            | Mem _ -> loop [] 0
    );
    Array.fold_left (+) 0 mem

let show : t -> unit = function
    | Mask xs ->
        (
            Printf.printf "Mask [";
            List.iter (fun (i, v) -> Printf.printf " (%d @ %d)," v i) xs;
            Printf.printf " ]\n"
        )
    | Mem (i, v) -> Printf.printf "Mem[%d] = %d\n" i v

let () : unit =
    let xs : t array =
        Prelude.read_file Sys.argv.(1)
        |> Prelude.split_newlines
        |> Array.map (fun x -> tokenize x |> parse) in
    let n : int = (get_max_address xs) + 1 in
    solve_1 xs n |> Printf.printf "%d\n"
