let validate (xs : int array) (n : int) (l : int) : bool =
    let m : int = n - 1 in
    let rec loop (i : int) (j : int) : bool =
        if i = m then
            false
        else if j = n then
            loop (i + 1) (i + 2)
        else if (xs.((l - i) - 1) + xs.((l - j) - 1)) = xs.(l) then
            true
        else
            loop i (j + 1) in
    loop 0 1

let find_1 (xs : int array) (n : int) : int =
    let rec loop (i : int) : int =
        if validate xs n i then
            loop (i + 1)
        else
            xs.(i) in
    loop n

let slice_min (xs : 'a array) (i : int) (j : int) : 'a =
    let rec loop (x : 'a) (i : int) (j : int) : 'a =
        if i = j then
            x
        else
            let x' : 'a = xs.(i) in
            if x' < x then
                loop x' (i + 1) j
            else
                loop x (i + 1) j in
    loop xs.(i) (i + 1) j

let slice_max (xs : 'a array) (i : int) (j : int) : 'a =
    let rec loop (x : 'a) (i : int) (j : int) : 'a =
        if i = j then
            x
        else
            let x' : 'a = xs.(i) in
            if x < x' then
                loop x' (i + 1) j
            else
                loop x (i + 1) j in
    loop xs.(i) (i + 1) j

let find_2 (xs : int array) (a : int) : int =
    let rec loop (b : int) (i : int) (j : int) : (int * int) =
        let b : int = b + xs.(j) in
        if a = b then
            (i, j)
        else if b < a then
            loop b i (j + 1)
        else
            let j : int = i + 1 in
            loop xs.(j) j (j + 1) in
    let (i, j) : (int * int) = loop xs.(0) 0 1 in
    let min : int = slice_min xs i j in
    let max : int = slice_max xs i j in
    min + max

let () : unit =
    let xs : int array =
        Prelude.read_file Sys.argv.(1)
        |> Prelude.split_newlines
        |> Array.map (fun x -> Prelude.str_to_int x |> Option.get) in
    let n : int = xs.(0) in
    let xs : int array = Array.sub xs 1 ((Array.length xs) - 1) in
    let x : int = find_1 xs n in
    List.iter (Printf.printf "%d\n") [x; find_2 xs x]
