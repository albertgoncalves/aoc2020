let or_flag (flags : int) (x : char) : int =
    if ('a' <= x) && (x <= 'z') then
        flags lor (1 lsl ((Char.code x) - (Char.code 'a')))
    else
        flags

let bit_width : int = (Char.code 'z') - (Char.code 'a')

let count_flags (flags : int) : int =
    let rec loop (n : int) (i : int) : int =
        if (bit_width < i) then
            n
        else if (1 land (flags lsr i)) = 1 then
            loop (n + 1) (i + 1)
        else
            loop n (i + 1) in
    loop 0 0

let all_flags : int =
    let rec loop (flags : int) (i : int) : int =
        if (bit_width < i) then
            flags
        else
            loop (flags lor (1 lsl i)) (i + 1) in
    loop 0 0

let parse_line (xs : string) (flags : int) (i : int) : (int * int) =
    let rec loop (flags : int) (i : int) : (int * int) =
        if xs.[i] = '\n' then
            (flags, i + 1)
        else
            loop (or_flag flags xs.[i]) (i + 1) in
    loop flags i

let parse_all_1 (xs : string) : int =
    let n : int = String.length xs in
    let rec loop (flags_prev : int) (m : int) (i : int) : int =
        let (flags, j) : (int * int) = parse_line xs 0 i in
        let flags : int = flags lor flags_prev in
        if j = n then
            m + (count_flags flags)
        else if xs.[j] = '\n' then
            loop 0 (m + (count_flags flags)) (j + 1)
        else
            loop flags m j in
    loop 0 0 0

let parse_all_2 (xs : string) : int =
    let n : int = String.length xs in
    let rec loop (flags_prev : int) (m : int) (i : int) : int =
        let (flags, j) : (int * int) = parse_line xs 0 i in
        let flags : int = flags land flags_prev in
        if j = n then
            m + (count_flags flags)
        else if xs.[j] = '\n' then
            loop all_flags (m + (count_flags flags)) (j + 1)
        else
            loop flags m j in
    loop all_flags 0 0

let () : unit =
    let xs : string = Prelude.read_file Sys.argv.(1) in
    List.iter
        (fun f -> f xs |> Printf.printf "%d\n")
        [parse_all_1; parse_all_2]
