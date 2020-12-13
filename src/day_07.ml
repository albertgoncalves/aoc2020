let tokenize (s : string) : string array =
    let n : int = String.length s in
    let xs : string Queue.t = Queue.create () in
    let rec loop (i : int) (j : int) : unit =
        if j = n then
            ()
        else
            let x : char = s.[j] in
            if (x = ' ') || (x = ',') then (
                if i <> j then (
                    Queue.add (String.sub s i (j - i)) xs
                );
                let j : int = j + 1 in
                loop j j
            ) else if x = '.' then (
                Queue.add (String.sub s i (j - i)) xs
            ) else
                loop i (j + 1) in
    loop 0 0;
    Queue.to_seq xs |> Array.of_seq

type t = {
    label : string;
    count : int;
}

let parse (xs : string array) : (string * t list) =
    let n : int = Array.length xs in
    assert (xs.(2) = "bags");
    assert (xs.(3) = "contain");
    let label : string = String.concat " " [xs.(0); xs.(1)] in
    let rec loop (ts : t list) (i : int) : t list =
        if xs.(i) = "no" then
            ts
        else (
            let child : t = {
                label = String.concat " " [xs.(i + 1); xs.(i + 2)];
                count = Prelude.str_to_int xs.(i) |> Option.get;
            } in
            let j : int = i + 4 in
            if (j < n) then (
                loop (child :: ts) j
            ) else
                child :: ts
        ) in
    (label, loop [] 4)

let get_table_1
        (xs : (string * t list) array) : (string, string list) Hashtbl.t =
    let table : (string, string list) Hashtbl.t =
        Hashtbl.create (Array.length xs) in
    let f (label : string) (child : t) : unit =
        match Hashtbl.find_opt table child.label with
            | None -> Hashtbl.add table child.label [label]
            | Some xs -> Hashtbl.replace table child.label (label :: xs) in
    Array.iter (fun (label, children) -> List.iter (f label) children) xs;
    table

let rec find_all_1
        (table : (string, string list) Hashtbl.t)
        (x : string) : string list =
    match Hashtbl.find_opt table x with
        | Some xs ->
            List.fold_left List.append xs (List.map (find_all_1 table) xs)
        | None -> []

type t' = (string, (string * int) list) Hashtbl.t

let get_table_2 (xs : (string * t list) array) : t' =
    let table : t' = Hashtbl.create (Array.length xs) in
    let f (label : string) (child : t) : unit =
        match Hashtbl.find_opt table label with
            | None -> Hashtbl.add table label [(child.label, child.count)]
            | Some xs ->
                Hashtbl.replace
                    table
                    label
                    ((child.label, child.count) :: xs) in
    Array.iter (fun (label, children) -> List.iter (f label) children) xs;
    table

let rec find_all_2 (table : t') ((x, n) : (string * int)) : int =
    match Hashtbl.find_opt table x with
        | Some xs ->
            List.fold_left (fun a b -> a + (n * (find_all_2 table b))) n xs
        | None -> n

let () : unit =
    let xs : (string * t list) array =
        Prelude.read_file Sys.argv.(1)
        |> Prelude.split_newlines
        |> Array.map (fun x -> tokenize x |> parse) in
    let x : string = "shiny gold" in
    find_all_1 (get_table_1 xs) x
    |> List.sort_uniq compare
    |> List.fold_left (fun x' _ -> x' + 1) 0
    |> Printf.printf "%d\n";
    find_all_2 (get_table_2 xs) (x, 1)
    |> (fun x' -> x' - 1)
    |> Printf.printf "%d\n"
