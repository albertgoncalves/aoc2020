let read_file (filename : string) : string =
    let stream : in_channel = open_in_bin filename in
    let size : int = in_channel_length stream in
    let buffer : Bytes.t = Bytes.create size in
    if (input stream buffer 0 size) <> size then (
        Printf.eprintf "read_file %S\n" filename;
        exit 1
    );
    Bytes.to_string buffer

let split_newlines (s : string) : string array =
    let xs : string Queue.t = Queue.create () in
    let f (i : int) (j : int) : unit =
        if i <> j then
            Queue.add (String.sub s i (j - i)) xs in
    let n : int = String.length s in
    let rec loop (i : int) (j : int) : unit =
        if j = n then
            f i j
        else if s.[j] = '\n' then (
            f i j;
            loop (j + 1) (j + 1)
        ) else
            loop i (j + 1) in
    loop 0 0;
    Queue.to_seq xs |> Array.of_seq

let is_alpha (x : char) : bool =
    (('A' <= x) && (x <= 'Z')) || (('a' <= x) && (x <= 'z'))

let is_digit (x : char) : bool =
    ('0' <= x) && (x <= '9')

let str_to_int (s : string) : int option =
    let rec loop (m : int) (e : int) (i : int) : int option =
        if i < 0 then
            Some m
        else
            let x : char = s.[i] in
            if is_digit x then
                let x : int = (Char.code x) - (Char.code '0') in
                loop (m + (x * e)) (e * 10) (i - 1)
            else
                None in
    loop 0 1 ((String.length s) - 1)

let str_to_char_array (s : string) : char array =
    Array.init (String.length s) (String.get s)
