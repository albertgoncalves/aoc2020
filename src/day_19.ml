type t =
    | Seq of int array
    | Alt of (int array * int array)
    | Lit of char

let parse_index (s : string) : (int * int) option =
    if Prelude.is_digit s.[0] then
        let rec loop (i : int) : (int * int) option =
            if s.[i] = ':' then
                let x : int =
                    String.sub s 0 i |> Prelude.str_to_int |> Option.get in
                Some (i, x)
            else
                loop (i + 1) in
        loop 0
    else
        None

let queue_to_array (xs : 'a Queue.t) : 'a array =
    Queue.to_seq xs |> Array.of_seq

let rec parse_rule (s : string) (i : int) : t =
    if s.[i] = '"' then
        Lit s.[i + 1]
    else
        let xs : int Queue.t = Queue.create () in
        let n : int = String.length s in
        let f (i : int) (j : int) : unit =
            if i <> j then (
                let x : int =
                    String.sub s i (j - i)
                    |> Prelude.str_to_int
                    |> Option.get in
                Queue.add x xs
            ) in
        let rec loop (i : int) (j : int) : t =
            if j = n then (
                f i j;
                Seq (queue_to_array xs)
            ) else
                match s.[j] with
                    | '|' ->
                        (
                            match parse_rule s (j + 1) with
                                | Seq xs' ->
                                    Alt (queue_to_array xs, xs')
                                | Alt _ | Lit _ ->
                                    (
                                        Printf.eprintf "parse_rule\n";
                                        exit 1
                                    )
                        )
                    | ' ' ->
                        (
                            if i <> j then (
                                f i j
                            );
                            let j : int = j + 1 in
                            loop j j
                        )
                    | _ -> loop i (j + 1) in
        loop i i

let parse (xs : string array) : ((int, t) Hashtbl.t * string array) =
    let n : int = Array.length xs in
    let rules : (int, t) Hashtbl.t = Hashtbl.create n in
    let rec loop (i : int) : int =
        let s : string = xs.(i) in
        match parse_index s with
            | Some (j, index) ->
                (
                    let rule : t = parse_rule s (j + 2) in
                    Hashtbl.add rules index rule;
                    loop (i + 1)
                )
            | None -> i in
    let i : int = loop 0 in
    (rules, Array.sub xs i (n - i))

let rec traverse
        (s : string)
        (i : int)
        (rules : (int, t) Hashtbl.t)
        (rule : t) : int list =
    if (String.length s) <= i then
        []
    else
        match rule with
            | Seq xs ->
                let xs : t array = Array.map (Hashtbl.find rules) xs in
                let m : int = Array.length xs in
                let rec loop (i : int) (j : int) : int list =
                    if j = m then
                        [i]
                    else
                        List.map
                            (fun i -> loop i (j + 1))
                            (traverse s i rules xs.(j))
                        |> List.concat in
                loop i 0
            | Alt (a, b) ->
                List.rev_append
                    (traverse s i rules (Seq a))
                    (traverse s i rules (Seq b))
            | Lit x ->
                if s.[i] = x then
                    [i + 1]
                else
                    []

let tally (rules : (int, t) Hashtbl.t) (s : string) : int =
    let n : int = String.length s in
    List.map
        (fun i -> if i = n then 1 else 0)
        (traverse s 0 rules (Hashtbl.find rules 0))
    |> List.fold_left (+) 0

let () : unit =
    let (rules, xs) : ((int, t) Hashtbl.t * string array) =
        Prelude.read_file Sys.argv.(1)
        |> Prelude.split_newlines
        |> parse in
    let f () : unit =
        Array.map (tally rules) xs
        |> Array.fold_left (+) 0
        |> Printf.printf "%d\n" in
    f ();
    Hashtbl.replace rules 8 (Alt ([|42|], [|42; 8|]));
    Hashtbl.replace rules 11 (Alt ([|42; 31|], [|42; 11; 31|]));
    f ()
