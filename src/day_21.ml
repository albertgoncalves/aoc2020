module StrSet = Set.Make (struct
    let compare = compare
    type t = string
end)

let push (xs : string list) (s : string) (i : int) (j : int) : string list =
    if i <> j then
        (String.sub s i (j - i)) :: xs
    else
        xs

let parse_parens (s : string) (i : int) : string list =
    let rec loop (xs : string list) (i : int) (j : int) : string list =
        match s.[j] with
            | ')' -> push xs s i j
            | ',' | ' ' ->
                let xs : string list = push xs s i j in
                let i : int = j + 1 in
                loop xs i i
            | _ -> loop xs i (j + 1) in
    loop [] i i

let parse (s : string) : (string list * string list) =
    let rec loop
            (xs : string list)
            (i : int)
            (j : int) : (string list * string list) =
        match s.[j] with
            | '(' ->
                (parse_parens s (j + 1) |> List.filter ((<>) "contains"), xs)
            | ' ' ->
                let xs : string list = push xs s i j in
                let i : int = j + 1 in
                loop xs i i
            | _ -> loop xs i (j + 1) in
    loop [] 0 0

let insert
        (table : (string, StrSet.t) Hashtbl.t)
        (value : string list)
        (keys : string list) : string list =
    let xs : StrSet.t = value |> StrSet.of_list in
    let f (key : string) : unit =
        match Hashtbl.find_opt table key with
            | None -> Hashtbl.add table key xs
            | Some xs' -> Hashtbl.replace table key (StrSet.inter xs xs') in
    List.iter f keys;
    value

let shrink (table : (string, StrSet.t) Hashtbl.t) (key : string) : unit =
    let value : StrSet.t = Hashtbl.find table key in
    if StrSet.cardinal value <> 1 then
        ()
    else
        let value : string = StrSet.choose value in
        let rec loop : string list -> unit = function
            | [] -> ()
            | x :: xs ->
                if key = x then
                    loop xs
                else
                    (
                        let prev : StrSet.t = Hashtbl.find table x in
                        Hashtbl.replace table x (StrSet.remove value prev);
                        loop xs
                    ) in
        loop (Hashtbl.to_seq_keys table |> List.of_seq)

let continue (table : (string, StrSet.t) Hashtbl.t) : bool =
    let rec loop : string list -> bool = function
        | [] -> false
        | x :: xs ->
            if Hashtbl.find table x |> StrSet.cardinal = 1 then
                loop xs
            else
                true in
    let keys : string list = Hashtbl.to_seq_keys table |> List.of_seq in
    List.iter (shrink table) keys;
    loop keys

let show (key : string) (value : StrSet.t) : unit =
    Printf.printf "{ %S, [ " key;
    StrSet.iter (Printf.printf "%S ") value;
    Printf.printf "] }\n"

let () : unit =
    let xs : (string list * string list) list =
        Prelude.read_file Sys.argv.(1)
        |> Prelude.split_newlines
        |> Array.map parse
        |> Array.to_list in
    let table : (string, StrSet.t) Hashtbl.t =
        Hashtbl.create (List.length xs) in
    let xs : string list =
        List.fold_left
            (fun x (k, v) -> List.rev_append x (insert table v k))
            []
            xs in
    while continue table do
        ()
    done;
    Hashtbl.iter show table;
    (
        let union : StrSet.t =
            Hashtbl.to_seq_values table
            |> Seq.fold_left (fun a b -> StrSet.union a b) StrSet.empty in
        List.filter (fun x -> StrSet.mem x union |> not) xs
        |> List.length
        |> Printf.printf "%d\n"
    );
    (
        Hashtbl.to_seq table
        |> List.of_seq
        |> List.map (fun (k, v) -> (k, StrSet.choose v))
        |> List.sort (fun (a, _) (b, _) -> compare a b)
        |> List.iter (fun (_, value) -> Printf.printf "%s," value);
        Printf.printf "\n"
    )
