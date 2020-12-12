let read_file (filename : string) : string =
    let stream : in_channel = open_in_bin filename in
    let size : int = in_channel_length stream in
    let buffer : Bytes.t = Bytes.create size in
    if (input stream buffer 0 size) <> size then (
        Printf.fprintf stderr "read_file %S\n" filename;
        exit 1
    );
    Bytes.to_string buffer

let split_newlines (s : string) : string array =
    Str.split (Str.regexp "\n+") s |> Array.of_list

let is_digit (x : char) : bool =
    ('0' <= x) && (x <= '9')

let str_to_int (xs : string) : int option =
    let rec loop (m : int) (e : int) (i : int) : int option =
        if i < 0 then
            Some m
        else
            let x : char = xs.[i] in
            if is_digit x then
                let x : int = (Char.code x) - (Char.code '0') in
                loop (m + (x * e)) (e * 10) (i - 1)
            else
                None in
    loop 0 1 ((String.length xs) - 1)
