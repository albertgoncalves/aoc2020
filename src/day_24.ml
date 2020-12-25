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

let convert : token -> coord = function
    | East -> { x = 2; y = 0 }
    | SouthEast -> { x = 1; y = -1 }
    | SouthWest -> { x = -1; y = -1 }
    | West -> { x = -2; y = 0 }
    | NorthWest -> { x = -1; y = 1 }
    | NorthEast -> { x = 1; y = 1 }

let combine (a : coord) (b : coord) : coord =
    { x = a.x + b.x; y = a.y + b.y }

let () : unit =
    Array.map convert [|East; SouthEast; West|]
    |> Array.fold_left combine { x = 0; y = 0 }
    |> (fun c -> Printf.printf "%d %d\n" c.x c.y)
