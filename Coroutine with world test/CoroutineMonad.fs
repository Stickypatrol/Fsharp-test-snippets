﻿module CoroutineMonad

open Math

type Coroutine<'w, 's, 'a> = 'w -> 's -> CoroutineStep<'w, 's, 'a>
and CoroutineStep<'w, 's, 'a> =
  | Done of 'a*'s
  | Yield of Coroutine<'w, 's, 'a>*'s

let rec bind (p: Coroutine<'w, 's, 'a>, k: 'a -> Coroutine<'w, 's, 'b>): Coroutine<'w, 's, 'b> =
    fun w s ->
      match p w s with
      | Done (x, s') -> k x w s'
      | Yield (p', s') -> Yield(bind(p', k), s')
let ret x = fun w s -> Done (x, s)

type CoroutineBuilder() =
  member this.Return(x: 'a): Coroutine<'w, 's, 'a> = ret x
  member this.ReturnFrom(s: Coroutine<'w, 's, 'a>) = s
  member this.Bind(p, k) = bind (p, k)
let cs = CoroutineBuilder()

let printX a = fun w s -> printfn "%A" a
                          Done((),s)

let rec repeat s =
  cs{
    do! s
    return! repeat s
  }

let yield_ = fun w s -> Yield((fun w s -> Done((),s)),s)

let wait interval =
  let time = fun w s -> Done(System.DateTime.Now, s)
  cs{
    let! t0 = time
    let rec wait() =
      cs{
        let! t = time
        let dt = (t-t0).TotalSeconds
        if dt > interval then
          do! yield_
          return ()
        else
          do! yield_
          return! wait()
      }
    do! wait()
  }

let co_step =
  function
  | Done(a, s) -> cs{return a}
  | Yield(p', s) -> p'

type Entity<'w, 'fs, 'dc> =
  {
    Fields            : 'fs
    Update            : Coroutine<'w, 'fs, Unit>
    Draw              : Coroutine<'w* 'fs, 'dc, Unit>
  }