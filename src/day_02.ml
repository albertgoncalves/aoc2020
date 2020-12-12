let parse_int (tokens : string) (i : int) : (int * int) =
    let rec loop (i : int) : int =
        if Prelude.is_digit tokens.[i] then
            loop (i + 1)
        else
            i in
    let j : int = loop i in
    (String.sub tokens i (j - i) |> Prelude.str_to_int |> Option.get, j)

let parse (tokens : string) : (string * char * int * int) =
    let (l, i) : (int * int) = parse_int tokens 0 in
    let (r, i) : (int * int) = parse_int tokens (i + 1) in
    let x : char = tokens.[i + 1] in
    let i : int = i + 4 in
    let n : int = String.length tokens in
    (String.sub tokens i (n - i), x, l, r)

let valid_1 (tokens : string) : bool =
    let (xs, x, l, r) : (string * char * int * int) = parse tokens in
    let tally : int ref = ref 0 in
    String.iter (fun x' -> if x = x' then incr tally) xs;
    (l <= !tally) && (!tally <= r)

let valid_2 (tokens : string) : bool =
    let (xs, x, l, r) : (string * char * int * int) = parse tokens in
    let a : char = xs.[l - 1] in
    let b : char = xs.[r - 1] in
    ((a = x) && (b <> x)) || ((a <> x) && (b = x))

let () : unit =
    let xs : string array =
        Prelude.read_file Sys.argv.(1) |> Prelude.split_newlines in
    let f (xs' : string array) (f' : string -> bool) : unit =
        Array.map (fun x -> if f' x then 1 else 0) xs'
        |> Array.fold_left (+) 0
        |> Printf.printf "%d\n" in
    List.iter (f xs) [valid_1; valid_2]
