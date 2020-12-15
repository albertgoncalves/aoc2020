type t = {
    mutable x : int;
    mutable y : int;
    mutable a : int;
}

let parse_1 (pos : t) (s : string) : unit =
    let n : int =
        (String.length s) - 1
        |> String.sub s 1
        |> Prelude.str_to_int
        |> Option.get in
    match s.[0] with
        | 'N' -> pos.y <- pos.y + n
        | 'S' -> pos.y <- pos.y - n
        | 'E' -> pos.x <- pos.x + n
        | 'W' -> pos.x <- pos.x - n
        | 'L' -> pos.a <- (pos.a + n) mod 360
        | 'R' -> pos.a <- ((360) + (pos.a - n)) mod 360
        | 'F' when pos.a = 0 -> pos.x <- pos.x + n
        | 'F' when pos.a = 90 -> pos.y <- pos.y + n
        | 'F' when pos.a = 180 -> pos.x <- pos.x - n
        | 'F' when pos.a = 270 -> pos.y <- pos.y - n
        | _ ->
            (
                Printf.eprintf "parse\n";
                exit 1
            )

let show_1 (xs : string array) : unit =
    let pos : t = {
        x = 0;
        y = 0;
        a = 0;
    } in
    Array.iter (parse_1 pos) xs;
    Printf.printf "%d\n" ((abs pos.x) + (abs pos.y))

type t' = {
    mutable px : int;
    mutable py : int;
    mutable wx : int;
    mutable wy : int;
}

let turn_left (pos : t') : unit =
    let x : int = pos.wx in
    let y : int = pos.wy in
    pos.wx <- y;
    pos.wy <- x * -1

let turn_right (pos : t') : unit =
    let x : int = pos.wx in
    let y : int = pos.wy in
    pos.wx <- y * -1;
    pos.wy <- x

let flip (pos : t') : unit =
    pos.wx <- pos.wx * -1;
    pos.wy <- pos.wy * -1

let parse_2 (pos : t') (s : string) : unit =
    let n : int =
        (String.length s) - 1
        |> String.sub s 1
        |> Prelude.str_to_int
        |> Option.get in
    match s.[0] with
        | 'N' -> pos.wy <- pos.wy + n
        | 'S' -> pos.wy <- pos.wy - n
        | 'E' -> pos.wx <- pos.wx + n
        | 'W' -> pos.wx <- pos.wx - n
        | 'L' | 'R' when n = 180 -> flip pos
        | 'L' when n = 90 -> turn_right pos
        | 'R' when n = 270 -> turn_right pos
        | 'L' when n = 270 -> turn_left pos
        | 'R' when n = 90 -> turn_left pos
        | 'F' ->
            (
                pos.px <- pos.px + (pos.wx * n);
                pos.py <- pos.py + (pos.wy * n)
            )
        | _ ->
            (
                Printf.eprintf "parse\n";
                exit 1
            )

let show_2 (xs : string array) : unit =
    let pos : t' = {
        px = 0;
        py = 0;
        wx = 10;
        wy = 1;
    } in
    Array.iter (parse_2 pos) xs;
    Printf.printf "%d\n" ((abs pos.px) + (abs pos.py))

let () : unit =
    let xs : string array =
        Prelude.read_file Sys.argv.(1) |> Prelude.split_newlines in
    show_1 xs;
    show_2 xs
