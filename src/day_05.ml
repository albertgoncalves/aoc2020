type t = {
    min : int;
    max : int;
}

let parse ((front_back, left_right) : (t * t)) : char -> (t * t) = function
    | 'F' ->
        let l : int = front_back.min in
        let r : int = front_back.max in
        let m : int = ((r - l) / 2) + l in
        ({min = l; max = m}, left_right)
    | 'B' ->
        let l : int = front_back.min in
        let r : int = front_back.max in
        let m : int = r - ((r - l) / 2) in
        ({min = m; max = r}, left_right)
    | 'L' ->
        let l : int = left_right.min in
        let r : int = left_right.max in
        let m : int = ((r - l) / 2) + l in
        (front_back, {min = l; max = m})
    | 'R' ->
        let l : int = left_right.min in
        let r : int = left_right.max in
        let m : int = r - ((r - l) / 2) in
        (front_back, {min = m; max = r})
    | _ -> (front_back, left_right)

let to_int ((front_back, left_right) : (t * t)) : int =
    let invalid : bool =
        (front_back.min <> front_back.max) ||
        (left_right.min <> left_right.max) in
    if invalid then (
        Printf.eprintf "to_int\n";
        exit 1
    ) else
        (front_back.min * 8) + left_right.min

let find_gap (xs : int list) : int =
    let superset : (int, unit) Hashtbl.t =
        let x : (int, unit) Hashtbl.t = Hashtbl.create 1024 in
        for i = 0 to 1023 do
            Hashtbl.add x i ();
        done;
        x in
    List.iter (Hashtbl.remove superset) xs;
    let xs : int list =
        Hashtbl.to_seq_keys superset |> List.of_seq |> List.sort compare in
    let rec loop : int list -> int = function
        | a :: b :: xs ->
            if 1 < (b - a) then
                b
            else
                loop (b :: xs)
        | _ ->
            (
                Printf.eprintf "find_gap\n";
                exit 1
            ) in
    loop xs

let str_to_char_array (s : string) : char array =
    Array.init (String.length s) (String.get s)

let () : unit =
    let xs : string list =
        Prelude.read_file Sys.argv.(1)
        |> Prelude.split_newlines
        |> Array.to_list in
    let f (x : string) : int =
        str_to_char_array x
        |> Array.fold_left parse ({min = 0; max = 127}, {min = 0; max = 7})
        |> to_int in
    let xs : int list = List.map f xs in
    List.fold_left max 0 xs |> Printf.printf "%d\n";
    find_gap xs |> Printf.printf "%d\n"
