type instr = Nop | Acc | Jmp

type halt = Normal | Loop

let parse (s : string) : (instr * int) =
    let x : instr = match String.sub s 0 3 with
        | "nop" -> Nop
        | "acc" -> Acc
        | "jmp" -> Jmp
        | _ ->
            (
                Printf.eprintf "parse instr\n";
                exit 1
            ) in
    let n : int option =
        String.sub s 5 ((String.length s) - 5) |> Prelude.str_to_int in
    let n : int = match n with
        | Some n -> n
        | None ->
            (
                Printf.eprintf "parse int\n";
                exit 1
            ) in
    if s.[4] = '+' then
        (x, n)
    else
        (x, n * -1)

let run (xs : (instr * int) array) : (halt * int) =
    let n : int = Array.length xs in
    let memory : (int, int) Hashtbl.t = Hashtbl.create n in
    let rec loop (x : int) (i : int) : (halt * int) =
        if i = n then
            (Normal, x)
        else
            match xs.(i) with
                | (Nop, _) -> loop x (i + 1)
                | (Jmp, n) -> loop x (i + n)
                | (Acc, n) ->
                    match Hashtbl.find_opt memory i with
                        | Some _ -> (Loop, x)
                        | None ->
                            (
                                let x : int = x + n in
                                Hashtbl.add memory i x;
                                loop x (i + 1)
                            ) in
    loop 0 0

let brute_force (xs : (instr * int) array) : (int * int) =
    let flip (i : int) : unit =
        match xs.(i) with
            | (Jmp, n) -> xs.(i) <- (Nop, n)
            | (Nop, n) -> xs.(i) <- (Jmp, n)
            | (Acc, _) -> () in
    let rec loop (i : int) : (int * int) =
        match xs.(i) with
            | (Nop, 0) | (Acc, _) -> loop (i + 1)
            | (Nop, _) | (Jmp, _) ->
                (
                    flip i;
                    match run xs with
                        | (Normal, n) -> (i, n)
                        | (Loop, _) ->
                            (
                                flip i;
                                loop (i + 1)
                            )
                ) in
    loop 0

let () : unit =
    let xs : (instr * int) array =
        Prelude.read_file Sys.argv.(1)
        |> Prelude.split_newlines
        |> Array.map parse in
    (
        match run xs with
            | (Normal, n) -> Printf.printf "Normal\tn:%d\n" n
            | (Loop, n) ->  Printf.printf "Loop\tn:%d\n" n
    );
    brute_force xs |> (fun (i, n) -> Printf.printf "i:%d\tn:%d\n" i n)
