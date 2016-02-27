type token =
  | A of string
  | B of int


let atoken = "stringy" |> A
let btoken = "stringy" |> A
let ctoken = 123 |> B

let main a b =
  if a = b then
    printfn "these are equal"
  ()

do main atoken btoken