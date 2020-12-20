type t =
    | Seq of int array
    | Alt of (int array * int array)
    | Lit of char

let rec traverse
        (s : string)
        (i : int)
        (rules : (int, t) Hashtbl.t)
        (rule : t) : int option =
    if (String.length s) <= i then
        None
    else
        match rule with
            | Seq xs ->
                let xs : t array = Array.map (Hashtbl.find rules) xs in
                let m : int = Array.length xs in
                let rec loop (i : int) (j : int) : int option =
                    if j = m then
                        Some i
                    else
                        match traverse s i rules xs.(j) with
                            | Some i -> loop i (j + 1)
                            | None -> None in
                loop i 0
            | Alt (a, b) ->
                (
                    match traverse s i rules (Seq a) with
                        | Some i -> Some i
                        | None -> traverse s i rules (Seq b)
                )
            | Lit x ->
                if s.[i] = x then
                    Some (i + 1)
                else
                    None

let () : unit =
    let rules : (int, t) Hashtbl.t = Hashtbl.create 6 in
    Array.iter
        (fun (i, r) -> Hashtbl.add rules i r)
        [|
            (4, Lit 'a');
            (5, Lit 'b');
            (1, Alt ([|2; 3|], [|3; 2|]));
            (2, Alt ([|4; 4|], [|5; 5|]));
            (3, Alt ([|4; 5|], [|5; 4|]));
            (0, Seq [|4; 1; 5|]);
        |];
    let f (s : string) : unit =
        let x : bool =
            match traverse s 0 rules (Hashtbl.find rules 0) with
                | Some i when i = String.length s -> true
                | _ -> false in
        Printf.printf "%-10S -> %b\n" s x in
    let xs : string array = [|
        "ababbb";
        "bababa";
        "abbbab";
        "aaabbb";
        "aaaabbb";
    |] in
    Array.iter f xs
