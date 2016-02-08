type Maybe<'a> =
  | Some of 'a
  | None

type Coroutine<'a, 's> = 's -> CoroutineStep<'a, 's>
and CoroutineStep<'a, 's> =
  | Done of Maybe<'a* 's>
  | Yield of Coroutine<'a, 's>*'s

let mayberet x = Some(x)

let maybebind p k =
  match p with
  | Some(x) -> k x
  | None -> None

type MaybeBuilder() =
  member this.Return x = mayberet x
  member this.ReturnFrom m = m
  member this.Bind(p,k) = maybebind p k
let maybe = MaybeBuilder()

let coroutineret x = fun s -> mayberet(x, s)

let coroutinebind (p:Coroutine<'a, 's>) (k:'a -> Coroutine<'b, 's>) : Coroutine<'b, 's> =
  fun s ->
    match p s with
    | Done(res) -> maybe{ let! a, s' = res
                          return! k a s'}
    | Yield(p', s') -> 