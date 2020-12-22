module StrSet = Set.Make(struct
    let compare = compare
    type t = string
end)

let insert
        (sets : (string, StrSet.t) Hashtbl.t)
        (value : string list)
        (keys : string list) : string list =
    let xs : StrSet.t = value |> StrSet.of_list in
    let f (key : string) : unit =
        match Hashtbl.find_opt sets key with
            | None -> Hashtbl.add sets key xs
            | Some xs' -> Hashtbl.replace sets key (StrSet.inter xs xs') in
    List.iter f keys;
    value

let show (key : string) (value : StrSet.t) : unit =
    Printf.printf "{ %S, [ " key;
    StrSet.iter (Printf.printf "%S ") value;
    Printf.printf "] }\n"

let solve (xs : (string list * string list) list) : string list =
    let sets : (string, StrSet.t) Hashtbl.t = Hashtbl.create 3 in
    let xs : string list =
        List.fold_left
            (fun x (k, v) -> List.rev_append x (insert sets v k))
            []
            xs in
    Hashtbl.iter show sets;
    let union : StrSet.t =
        Hashtbl.to_seq_values sets
        |> Seq.fold_left (fun a b -> StrSet.union a b) StrSet.empty in
    List.filter (fun x -> StrSet.mem x union |> not) xs

let () : unit =
    let xs : (string list * string list) list =
        [
            (["dairy"; "fish"], ["mxmxvkd"; "kfcds"; "sqjhc"; "nhms"]);
            (["dairy"], ["trh"; "fvjkl"; "sbzzf"; "mxmxvkd"]);
            (["soy"], ["sqjhc"; "fvjkl"]);
            (["fish"], ["sqjhc"; "mxmxvkd"; "sbzzf"]);
        ] in
    solve xs |> List.iter (Printf.printf "%S\n")
