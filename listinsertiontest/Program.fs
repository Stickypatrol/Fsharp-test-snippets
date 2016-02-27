let a = [1;2;3;4;5;8;9;10]
let b = [6;7]

let rec insertor x y =
  match x with
  | [] -> []
  | h::t -> if h = 5 then
              ((h::y)@(insertor t y))
            else
              h::(insertor t y)
printfn "%A" (insertor a b)