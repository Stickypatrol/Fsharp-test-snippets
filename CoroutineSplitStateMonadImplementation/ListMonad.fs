module ListMonad

type list<'a> =
  | Node of 'a * list<'a>
  | Empty

let ret x = Node(x, Empty)

let rec bind p k =
  match p with
  | h::t -> (k h) @ (bind t k)
  | [] -> []

type ListBuilder() =
  member this.Return x = ret x
  member this.Bind(p,k) = bind p k
let lst = ListBuilder()