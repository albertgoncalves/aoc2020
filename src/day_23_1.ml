type t = {
    value : int;
    mutable next : t option;
}

let show (x : t) : unit =
    let rec loop : t option -> unit = function
        | None -> ()
        | Some x' ->
            if x.value <> x'.value then (
                Printf.printf "%d " x'.value;
                loop x'.next
            ) in
    Printf.printf "[ %d " x.value;
    loop x.next;
    Printf.printf "]\n"

let get_table (xs : int array) : t array =
    let n : int = Array.length xs in
    let m : int = n - 1 in
    let table : t array =
        Array.init n (fun i -> { value = (i + 1); next = None}) in
    for i = 0 to (m - 1) do
        table.(xs.(i) - 1).next <- Some (table.(xs.(i + 1) - 1))
    done;
    table.(xs.(m) - 1).next <- Some (table.(xs.(0) - 1));
    table

let slice_3 (x : t) : (t * t) =
    let rec loop (n : int) (x : t) : t =
        if n = 0 then
            x
        else
            loop (n - 1) (x.next |> Option.get) in
    let a : t = x in
    let b : t = a.next |> Option.get in
    let c : t = loop 2 b in
    let d : t = c.next |> Option.get in
    a.next <- Some d;
    c.next <- Some b;
    (b, c)

let inject (x : t) (front : t) (back : t) : unit =
    let a : t = x in
    let b : t = a.next |> Option.get in
    a.next <- Some front;
    back.next <- Some b

let rec find (target : int) (n : int) (x : t) : bool =
    if x.value = target then
        true
    else if n <= 0 then
        false
    else
        find target (n - 1) (x.next |> Option.get)

let run (x : t) (m : int) (table : t array) : t =
    let (front, back) : (t * t) = slice_3 x in
    let value : int ref =
        let v : int = (m + (x.value - 1)) mod m in
        if v = 0 then
            ref m
        else
            ref v in
    while find !value 2 front do
        value :=
            let v : int = (m + (!value - 1)) mod m in
            if v = 0 then
                m
            else
                v
    done;
    inject (table.(!value - 1)) front back;
    x.next |> Option.get

let solve_1 (m : int) (xs : int array) : unit =
    let table : t array = get_table xs in
    let rec loop (x : t) (n : int) : unit =
        if n <= 0 then
            ()
        else
            loop (run x 9 table) (n - 1) in
    loop table.(xs.(0) - 1) m;
    table.(0) |> show

let solve_2 (m : int) (xs : int array) : unit =
    let xs : int array =
        Array.append xs (Array.init (1000000 - 9) (fun i -> i + 10)) in
    let table : t array = get_table xs in
    let rec loop (x : t) (n : int) : unit =
        if n <= 0 then
            ()
        else
            loop (run x 1000000 table) (n - 1) in
    loop (table.(xs.(0) - 1)) m;
    let a : t = table.(0).next |> Option.get in
    let b : t = a.next |> Option.get in
    Printf.printf "%d\n" (a.value * b.value)

let () : unit =
    let xs : int array array =
        [|
            [|3; 8; 9; 1; 2; 5; 4; 6; 7|];
            [|8; 7; 2; 4; 9; 5; 1; 3; 6|];
        |] in
    Array.iter (solve_1 100) xs;
    Array.iter (solve_2 10000000) xs
