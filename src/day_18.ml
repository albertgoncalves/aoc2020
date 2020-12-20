type t =
    | LParen
    | RParen
    | OpAdd
    | OpMul
    | Int of int

let tokenize (s : string) : t Queue.t =
    let xs : t Queue.t = Queue.create () in
    let n : int = String.length s in
    let f (i : int) (j : int) : unit =
        if i <> j then
            match String.sub s i (j - i) |> Prelude.str_to_int with
                | Some x -> Queue.add (Int x) xs
                | None -> () in
    let rec loop (i : int) (j : int) : unit =
        if j = n then (
            f i j;
            ()
        ) else
            match s.[j] with
                | '(' ->
                    (
                        f i j;
                        Queue.add LParen xs;
                        let i = j + 1 in
                        loop i i
                    )
                | ')' ->
                    (
                        f i j;
                        Queue.add RParen xs;
                        let i = j + 1 in
                        loop i i
                    )
                | '+' ->
                    (
                        f i j;
                        Queue.add OpAdd xs;
                        let i = j + 1 in
                        loop i i
                    )
                | '*' ->
                    (
                        f i j;
                        Queue.add OpMul xs;
                        let i = j + 1 in
                        loop i i
                    )
                | ' ' ->
                    (
                        f i j;
                        let i = j + 1 in
                        loop i i
                    )
                | _ -> loop i (j + 1) in
    loop 0 0;
    xs

let rec parse_1 (xs : t Queue.t) : int =
    let x : int ref = ref 0 in
    let continue : bool ref = ref true in
    while !continue do
        match Queue.take_opt xs with
            | None -> continue := false
            | Some token ->
                match token with
                    | LParen -> x := parse_1 xs
                    | RParen -> continue := false
                    | OpAdd -> x := !x + (get_int_1 xs)
                    | OpMul -> x := !x * (get_int_1 xs)
                    | Int x' -> x := x'
    done;
    !x

and get_int_1 (xs : t Queue.t) : int =
    match Queue.take xs with
        | Int x -> x
        | LParen -> parse_1 xs
        | RParen | OpAdd | OpMul ->
            (
                Printf.eprintf "get_int\n";
                exit 1
            )

let rec parse_2 (xs : t Queue.t) : int =
    let x : int ref = ref 0 in
    let continue : bool ref = ref true in
    while !continue do
        match Queue.take_opt xs with
            | None -> continue := false
            | Some token ->
                match token with
                    | LParen -> x := parse_2 xs
                    | RParen -> continue := false
                    | OpAdd -> x := !x + (get_int_2 xs)
                    | OpMul ->
                        (
                            x := !x * (parse_2 xs);
                            continue := false
                        )
                    | Int x' -> x := x'
    done;
    !x

and get_int_2 (xs : t Queue.t) : int =
    match Queue.take xs with
        | Int x -> x
        | LParen -> parse_2 xs
        | RParen | OpAdd | OpMul ->
            (
                Printf.eprintf "get_int\n";
                exit 1
            )

let () : unit =
    let xs : string array =
        Prelude.read_file Sys.argv.(1) |> Prelude.split_newlines in
    let f (f' : t Queue.t -> int) : unit =
        Array.map (fun x -> tokenize x |> f') xs
        |> Array.fold_left (+) 0
        |> Printf.printf "%d\n" in
    Array.iter f [|parse_1; parse_2|]
