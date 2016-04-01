type list<'a> =
  | Node of 'a * list<'a>
  | Empty

type ListMonad() =
  member this.Return(x) = [x]
  member this.Bind(p:List<'a>,k:'a ->List<'b>) : List<'b> =
    match p with
    | [] -> []
    | h::t -> (k h) @ (this.Bind(t, k))
let Mlist = ListMonad()

type Maybe<'a> =
  | Some of 'a
  | None

(*type MaybeMonad() =
  member this.Return(x) = Some(x)
  member this.Bind(p:Maybe<'a>,k:'a -> Maybe<'b>) : Maybe<'b> =
    match p with
    | Some(x) -> ListMonad().Bind(x, (fun pres -> k pres))
    | None -> None
let maybe = MaybeMonad()*)