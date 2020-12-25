type token =
    | East
    | SouthEast
    | SouthWest
    | West
    | NorthWest
    | NorthEast

type coord = {
    x : int;
    y : int;
}

let parse (s : string) : token Queue.t =
    let n : int = String.length s in
    let xs : token Queue.t = Queue.create () in
    let rec loop (i : int) : unit =
        if i = n then
            ()
        else
            match s.[i] with
                | 'e' ->
                    (
                        Queue.add East xs;
                        loop (i + 1)
                    )
                | 's' when s.[i + 1] = 'e' ->
                    (
                        Queue.add SouthEast xs;
                        loop (i + 2)
                    )
                | 's' when s.[i + 1] = 'w' ->
                    (
                        Queue.add SouthWest xs;
                        loop (i + 2)
                    )
                | 'w' ->
                    (
                        Queue.add West xs;
                        loop (i + 1)
                    )
                | 'n' when s.[i + 1] = 'e' ->
                    (
                        Queue.add NorthEast xs;
                        loop (i + 2)
                    )
                | 'n' when s.[i + 1] = 'w' ->
                    (
                        Queue.add NorthWest xs;
                        loop (i + 2)
                    )
                | _ ->
                    (
                        Printf.eprintf "parse\n";
                        exit 1
                    ) in
    loop 0;
    xs

let convert : token -> coord = function
    | East -> { x = 2; y = 0 }
    | SouthEast -> { x = 1; y = -1 }
    | SouthWest -> { x = -1; y = -1 }
    | West -> { x = -2; y = 0 }
    | NorthWest -> { x = -1; y = 1 }
    | NorthEast -> { x = 1; y = 1 }

let (|+) (a : coord) (b : coord) : coord =
    { x = a.x + b.x; y = a.y + b.y }

let eval (s : string) : coord =
    parse s
    |> Queue.to_seq
    |> Seq.map convert
    |> Seq.fold_left (|+) { x = 0; y = 0 }

let get_mem (xs : string array) : (coord, bool) Hashtbl.t =
    let n : int = Array.length xs in
    let mem : (coord, bool) Hashtbl.t = Hashtbl.create n in
    let f (s : string) : unit =
        let c : coord = eval s in
        match Hashtbl.find_opt mem c with
            | None -> Hashtbl.add mem c true
            | Some x -> Hashtbl.replace mem c (not x) in
    Array.iter f xs;
    mem

let bool_to_int (x : bool) : int =
    if x then
        1
    else
        0

let tally (mem : (coord, bool) Hashtbl.t) : int =
    Hashtbl.to_seq_values mem |> Seq.map bool_to_int |> Seq.fold_left (+) 0

let global_neighbors : (coord, coord array) Hashtbl.t =
    Hashtbl.create 2048

let get_neighbors (c : coord) : coord array =
    match Hashtbl.find_opt global_neighbors c with
        | Some xs -> xs
        | None ->
            (
                let xs : token array =
                    [|
                        East;
                        SouthEast;
                        SouthWest;
                        West;
                        NorthWest;
                        NorthEast;
                    |] in
                let xs : coord array =
                    Array.map convert xs |> Array.map ((|+) c) in
                Hashtbl.add global_neighbors c xs;
                xs
            )

let count_neighbors
        (mem : (coord, bool) Hashtbl.t) : coord array -> bool array =
    let f (c : coord) : bool =
        match Hashtbl.find_opt mem c with
            | None -> false
            | Some x -> x in
    Array.map f

let fill_neighbors (mem : (coord, bool) Hashtbl.t) : unit =
    let f (c : coord) : unit =
        match Hashtbl.find_opt mem c with
            | None -> Hashtbl.add mem c false
            | Some _ -> () in
    Hashtbl.to_seq_keys mem
    |> Array.of_seq
    |> Array.map get_neighbors
    |> Array.iter (Array.iter f)

let run (mem : (coord, bool) Hashtbl.t) : unit =
    let f (c : coord) : (coord * bool) option =
        let n : int =
            get_neighbors c
            |> count_neighbors mem
            |> Array.map bool_to_int
            |> Array.fold_left (+) 0 in
        let black : bool = Hashtbl.find mem c in
        if black && ((n = 0) || (2 < n)) then
            Some (c, false)
        else if (not black) && (n = 2) then
            Some (c, true)
        else
            None in
    let f' : (coord * bool) option -> unit = function
        | None -> ()
        | Some (c, x) -> Hashtbl.replace mem c x in
    Hashtbl.to_seq_keys mem
    |> Array.of_seq
    |> Array.map f
    |> Array.iter f'

let () : unit =
    let mem : (coord, bool) Hashtbl.t =
        Prelude.read_file Sys.argv.(1)
        |> Prelude.split_newlines
        |> get_mem in
    let f () : unit = tally mem |> Printf.printf "%d\n%!" in
    f ();
    for _ = 1 to 100 do
        fill_neighbors mem;
        run mem
    done;
    f ()
