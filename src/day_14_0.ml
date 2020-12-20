type t' =
    | Zero
    | One
    | Floating

type t =
    | Mask of (int * t') list
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
            let rec loop (i : int) : (int * t') list =
                if i = n then
                    []
                else
                    let x : t' = match s.[i] with
                        | '0' -> Zero
                        | '1' -> One
                        | _ -> Floating in
                    ((n - 1) - i, x) :: loop (i + 1) in
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

let apply_mask_1 (x : int) : (int * t') -> int = function
    | (i, Zero) -> x land (lnot (1 lsl i))
    | (i, One) -> x lor (1 lsl i)
    | (_, Floating) -> x

let solve_1 (xs : t array) (n : int) : int =
    let mem : int array = Array.make n 0 in
    let m : int = Array.length xs in
    let rec loop (xs' : (int * t') list) (i : int) : unit =
        if i = m then
            ()
        else
            match xs.(i) with
                | Mask xs' -> loop xs' (i + 1)
                | Mem (j, v) ->
                    (
                        mem.(j) <- List.fold_left apply_mask_1 v xs';
                        loop xs' (i + 1)
                    ) in
    loop [] 0;
    Array.fold_left (+) 0 mem

let apply_mask_2_zero_one (x : int) : (int * t') -> int = function
    | (_, Zero) | (_, Floating) -> x
    | (i, One) -> x lor (1 lsl i)

let apply_mask_2_floating (mask : (int * t')) (x : int) : int list =
    match mask with
        | (_, Zero) -> [x]
        | (_, One) -> [x]
        | (i, Floating) -> [x land (lnot (1 lsl i)); x lor (1 lsl i)]

let apply_mask_2 (xs : (int * t') list) (x : int) : int list =
    List.fold_left
        (fun a b -> List.map (apply_mask_2_floating b) a |> List.concat)
        [List.fold_left apply_mask_2_zero_one x xs]
        xs

let solve_2 (xs : t array) : int =
    let n : int = Array.length xs in
    let mem : (int, int) Hashtbl.t = Hashtbl.create n in
    let rec loop (f : int -> int list) (i : int) : unit =
        if i = n then
            ()
        else
            match xs.(i) with
                | Mask xs' -> loop (apply_mask_2 xs') (i + 1)
                | Mem (j, v) ->
                    (
                        List.iter (fun j' -> Hashtbl.replace mem j' v) (f j);
                        loop f (i + 1)
                    ) in
    loop (fun x -> [x]) 0;
    Hashtbl.to_seq_values mem |> Array.of_seq |> Array.fold_left (+) 0

let () : unit =
    let xs : t array =
        Prelude.read_file Sys.argv.(1)
        |> Prelude.split_newlines
        |> Array.map (fun x -> tokenize x |> parse) in
    let n : int = (get_max_address xs) + 1 in
    List.iter (Printf.printf "%d\n") [solve_1 xs n; solve_2 xs]
