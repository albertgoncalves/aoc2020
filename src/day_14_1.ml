type t' = (string * int)

type t =
    | Mask of string
    | Mem of t'

let n : int = 36

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

let int_to_binary (x : int) : string =
    let xs : Buffer.t = Buffer.create n in
    for i = n - 1 downto 0 do
        if ((x lsr i) land 1) = 1 then
            Buffer.add_char xs '1'
        else
            Buffer.add_char xs '0'
    done;
    Buffer.contents xs

let parse (xs : string array) : t =
    match xs.(0) with
        | "mask" -> Mask xs.(1)
        | "mem" ->
            let f (i : int) : int = Prelude.str_to_int xs.(i) |> Option.get in
            Mem (f 1 |> int_to_binary, f 2)
        | _ ->
            (
                Printf.eprintf "parse\n";
                exit 1
            )

let combine (mask : string) (address : string) : string =
    let xs : Buffer.t = Buffer.create n in
    for i = 0 to n - 1 do
        match (mask.[i], address.[i]) with
            | ('0', x) -> Buffer.add_char xs x
            | ('1', _) -> Buffer.add_char xs '1'
            | ('X', _) -> Buffer.add_char xs 'X'
            | _ ->
                (
                    Printf.eprintf "combine\n";
                    exit 1
                )
    done;
    Buffer.contents xs

let get_addresses (xs : t array) : (t' array) =
    let xs' : t' Queue.t = Queue.create () in
    let n : int = Array.length xs in
    let rec loop (mask : string) (i : int) : unit =
        if i = n then
            ()
        else
            match xs.(i) with
                | Mask m -> loop m (i + 1)
                | Mem (address, value) ->
                    (
                        Queue.add (combine mask address, value) xs';
                        loop mask (i + 1)
                    ) in
    loop "000000000000000000000000000000000000" 0;
    Queue.to_seq xs' |> Array.of_seq

let get_intersection (a : string) (b : string) : string option =
    let xs : Buffer.t = Buffer.create n in
    let rec loop (i : int) : string option =
        if i = n then
            Some (Buffer.contents xs)
        else
            match (a.[i], b.[i]) with
                | ('X', '0') | ('0', 'X') ->
                    (
                        Buffer.add_char xs '0';
                        loop (i + 1)
                    )
                | ('X', '1') | ('1', 'X') ->
                    (
                        Buffer.add_char xs '1';
                        loop (i + 1)
                    )
                | (a', b') when a' = b' ->
                    (
                        Buffer.add_char xs a';
                        loop (i + 1)
                    )
                | _ -> None in
    loop 0

let set_negative_overlap (xs : t' Queue.t) (a : string) ((b, v) : t') : unit =
    match get_intersection a b with
        | Some x -> Queue.add (x, -v) xs
        | None -> ()

let get_overlaps (xs : t' array) : t' array =
    let xs' : t' Queue.t = Queue.create () in
    let buffer : t' Queue.t = Queue.create () in
    for i = 0 to (Array.length xs) - 1 do
        let (a, v) : t' = xs.(i) in
        Queue.iter (set_negative_overlap buffer a) xs';
        Queue.transfer buffer xs';
        Queue.add (a, v) xs'
    done;
    Queue.to_seq xs' |> Array.of_seq

let get_total ((address, value) : t') : int =
    let rec loop (x : int) (i : int) : int =
        if i = n then
            x
        else if address.[i] = 'X' then
            loop (x + 1) (i + 1)
        else
            loop x (i + 1) in
    (1 lsl (loop 0 0)) * value

let () : unit =
    Prelude.read_file Sys.argv.(1)
    |> Prelude.split_newlines
    |> Array.map (fun x -> tokenize x |> parse)
    |> get_addresses
    |> get_overlaps
    |> Array.map get_total
    |> Array.fold_left (+) 0
    |> Printf.printf "%d\n"
