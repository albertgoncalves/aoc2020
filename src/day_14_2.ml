(* NOTE: Wouldn't have been able to work out this approach without some
   *serious* help:
   `https://github.com/gr-g/advent-of-code-2020/blob/7fe81abb993e401a507cfe3be8a34247a7e395b0/src/bin/14.rs` *)

type t =
    | Mask of (int * int)
    | Mem of (int * int)

let tokenize (s : string) : string array =
    let xs : string Queue.t = Queue.create () in
    let f (i : int) (j : int) : unit =
        if i <> j then
            Queue.add (String.sub s i (j - i)) xs in
    let n : int = String.length s in
    let rec loop (i : int) (j : int) : unit =
        if j = n then
            f i j
        else
            let x : char = s.[j] in
            if not ((Prelude.is_alpha x) || (Prelude.is_digit x)) then (
                f i j;
                loop (j + 1) (j + 1)
            ) else
                loop i (j + 1) in
    loop 0 0;
    Queue.to_seq xs |> Array.of_seq

let n : int = 36

let parse (xs : string array) : t =
    match xs.(0) with
        | "mask" ->
            let s : string = xs.(1) in
            let rec loop (zeros : int) (ones : int) (i : int) : (int * int) =
                if i = n then
                    (zeros, ones)
                else
                    match s.[i] with
                        | '0' ->
                            loop ((1 lsl ((n - i) - 1)) lor zeros) ones (i + 1)
                        | '1' ->
                            loop zeros ((1 lsl ((n - i) - 1)) lor ones) (i + 1)
                        | _ -> loop zeros ones (i + 1) in
            Mask (loop 0 0 0)
        | "mem" ->
            let f (i : int) : int = Prelude.str_to_int xs.(i) |> Option.get in
            Mem (f 1, f 2)
        | _ ->
            (
                Printf.eprintf "parse\n";
                exit 1
            )

let solve_1 (xs : t array) : int =
    let n : int = Array.length xs in
    let mem : (int, int) Hashtbl.t = Hashtbl.create n in
    let rec loop (zeros : int) (ones : int) (i : int) : unit =
        if i = n then
            ()
        else
            match xs.(i) with
                | Mask (zeros, ones) -> loop zeros ones (i + 1)
                | Mem (address, value) ->
                    (
                        let value : int = (value lor ones) land (lnot zeros) in
                        Hashtbl.replace mem address value;
                        loop zeros ones (i + 1)
                    ) in
    loop 0 0 0;
    Hashtbl.to_seq_values mem |> Array.of_seq |> Array.fold_left (+) 0

let intersect
        ((a1, m1) : (int * int))
        ((a2, m2) : (int * int)) : (int * int) option =
    if ((a1 lxor a2) land (m1 land m2)) = 0 then
        Some (a1 lor a2, m1 lor m2)
    else
        None

let count_ones (x : int) : int =
    let rec loop (m : int) (i : int) : int =
        if i = n then
            m
        else if ((x lsr i) land 1) = 1 then
            loop (m + 1) (i + 1)
        else
            loop m (i + 1) in
    loop 0 0

let solve_2 (xs : t array) : int =
    let n : int = Array.length xs in
    let mem : (int * int * int) Queue.t = Queue.create () in
    let buffer : (int * int * int) Queue.t = Queue.create () in
    let f (a0 : int) (m0 : int) ((a1, m1, v1) : (int * int * int)) : unit =
        match intersect (a0, m0) (a1, m1) with
            | None -> ()
            | Some (a, m) -> Queue.add (a, m, -v1) buffer in
    let rec loop (zeros : int) (ones : int) (i : int) : unit =
        if i = n then
            ()
        else
            match xs.(i) with
                | Mask (zeros, ones) -> loop zeros ones (i + 1)
                | Mem (address, value) ->
                    (
                        let mask : int = zeros lor ones in
                        let address : int = (address lor ones) land mask in
                        Queue.iter (f address mask) mem;
                        Queue.transfer buffer mem;
                        Queue.add (address, mask, value) mem;
                        loop zeros ones (i + 1)
                    ) in
    loop 0 0 0;
    Queue.fold
        (fun x (_, m, v) -> ((1 lsl (lnot m |> count_ones)) * v) + x)
        0
        mem

let print_binary (x : int) : unit =
    for i = n - 1 downto 0 do
        (x lsr i) land 1 |> Printf.printf "%d"
    done;
    Printf.printf "\n"

let show : t -> unit = function
    | Mask (zeros, ones) ->
        (
            Printf.printf "Mask\n";
            print_binary zeros;
            print_binary ones;
            Printf.printf "\n"
        )
    | Mem (address, value) ->
        (
            Printf.printf "Mem\n";
            print_binary address;
            print_binary value;
            Printf.printf "\n"
        )

let () : unit =
    let xs : t array =
        Prelude.read_file Sys.argv.(1)
        |> Prelude.split_newlines
        |> Array.map (fun x -> tokenize x |> parse) in
    List.iter (Printf.printf "%d\n") [solve_1 xs; solve_2 xs]
