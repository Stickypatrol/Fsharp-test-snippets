type A  = int

type OneorMany =
  | One of A
  | Many of A * OneorMany

let x = Many(1,Many(2,Many(2,One(4))))