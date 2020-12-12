let rec until (xs : string) (f : char -> bool) (i : int) : int =
    if f xs.[i] then
        i
    else
        until xs f (i + 1)

let get_key (xs : string) (i : int) : (string * int) =
    let j : int = until xs ((=) ':') i in
    (String.sub xs i (j - i), j + 1)

let get_value (xs : string) (i : int) : (string * int) =
    let j : int = until xs (fun x -> (x = ' ') || (x = '\n')) i in
    (String.sub xs i (j - i), j + 1)

let validate (flags : int) : int =
    if (flags = 127) || (flags = 255) then
        1
    else
        0

let or_flag_1 (flags : int) : string -> int = function
    | "byr" -> flags lor (1 lsl 0)
    | "iyr" -> flags lor (1 lsl 1)
    | "eyr" -> flags lor (1 lsl 2)
    | "hgt" -> flags lor (1 lsl 3)
    | "hcl" -> flags lor (1 lsl 4)
    | "ecl" -> flags lor (1 lsl 5)
    | "pid" -> flags lor (1 lsl 6)
    | "cid" -> flags lor (1 lsl 7)
    | _ -> flags

let parse_pairs_1 (xs : string) : int -> (int * int) =
    let n : int = String.length xs in
    let rec loop (flags : int) (i : int) : (int * int) =
        let (key, i) : (string * int) = get_key xs i in
        let (_, i) : (string * int) = get_value xs i in
        let flags : int = or_flag_1 flags key in
        if (i = n) || (xs.[i] = '\n') then
            (flags, i)
        else
            loop flags i in
    loop 0

let parse_all_1 (xs : string) : int =
    let n : int = String.length xs in
    let rec loop (x : int) (i : int) : int =
        let (flags, i) : (int * int) = parse_pairs_1 xs i in
        let x : int = x + (validate flags) in
        if (i = n) then
            x
        else
            loop x (i + 1) in
    loop 0 0

let birth_year (flags : int) (value : string) : int =
    match Prelude.str_to_int value with
        | Some year when (1920 <= year) && (year <= 2002) ->
            flags lor (1 lsl 0)
        | _ -> flags

let issue_year (flags : int) (value : string) : int =
    match Prelude.str_to_int value with
        | Some year when (2010 <= year) && (year <= 2020) ->
            flags lor (1 lsl 1)
        | _ -> flags

let expir_year (flags : int) (value : string) : int =
    match Prelude.str_to_int value with
        | Some year when (2020 <= year) && (year <= 2030) ->
            flags lor (1 lsl 2)
        | _ -> flags

let height (flags : int) (value : string) : int =
    let n : int = String.length value in
    let suffix : string = String.sub value (n - 2) 2 in
    match Prelude.str_to_int (String.sub value 0 (n - 2)) with
        | None -> flags
        | Some number ->
            match suffix with
                | "cm" when (150 <= number) && (number <= 193) ->
                    flags lor (1 lsl 3)
                | "in" when (59 <= number) && (number <= 76) ->
                    flags lor (1 lsl 3)
                | _ -> flags

let hair_color (flags : int) (value : string) : int =
    let n : int = String.length value in
    if (n <> 7) || value.[0] <> '#' then
        flags
    else
        let rec loop (i : int) : bool =
            if i = n then
                true
            else
                let x : char = value.[i] in
                if (Prelude.is_digit x) || (('a' <= x) && (x <= 'f')) then
                    loop (i + 1)
                else
                    false in
        if loop 1 then
            flags lor (1 lsl 4)
        else
            flags

let eye_color (flags : int) (value : string) : int =
    let valid : bool = (value = "amb") ||
                       (value = "blu") ||
                       (value = "brn") ||
                       (value = "gry") ||
                       (value = "grn") ||
                       (value = "hzl") ||
                       (value = "oth") in
    if valid then
        flags lor (1 lsl 5)
    else
        flags

let passport_id (flags : int) (value : string) : int =
    let n : int = String.length value in
    if n <> 9 then
        flags
    else
        let rec loop (i : int) : bool =
            if i = n then
                true
            else if Prelude.is_digit value.[i] then
                loop (i + 1)
            else
                false in
        if loop 0 then
            flags lor (1 lsl 6)
        else
            flags

let or_flag_2 (flags : int) (key : string) (value : string) : int =
    match key with
        | "byr" -> birth_year flags value
        | "iyr" -> issue_year flags value
        | "eyr" -> expir_year flags value
        | "hgt" -> height flags value
        | "hcl" -> hair_color flags value
        | "ecl" -> eye_color flags value
        | "pid" -> passport_id flags value
        | "cid" -> flags lor (1 lsl 7)
        | _ -> flags

let parse_pairs_2 (xs : string) : int -> (int * int) =
    let n : int = String.length xs in
    let rec loop (flags : int) (i : int) : (int * int) =
        let (key, i) : (string * int) = get_key xs i in
        let (value, i) : (string * int) = get_value xs i in
        let flags : int = or_flag_2 flags key value in
        if (i = n) || (xs.[i] = '\n') then
            (flags, i)
        else
            loop flags i in
    loop 0

let parse_all_2 (xs : string) : int =
    let n : int = String.length xs in
    let rec loop (x : int) (i : int) : int =
        let (flags, i) : (int * int) = parse_pairs_2 xs i in
        let x : int = x + (validate flags) in
        if (i = n) then
            x
        else
            loop x (i + 1) in
    loop 0 0

let () : unit =
    let xs : string = Prelude.read_file Sys.argv.(1) in
    List.iter
        (fun f -> f xs |> Printf.printf "%d\n")
        [parse_all_1; parse_all_2]
