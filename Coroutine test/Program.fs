module Program

type Coroutine<'a, 's> = 's -> CoroutineStep<'a, 's>
and CoroutineStep<'a, 's> =
  | Done of 'a*'s
  | Yield of Coroutine<'a, 's>*'s


type CoroutineBuilder() =
  member this.Return(x: 'a): Coroutine<'a, 's> =
    fun s -> Done (x, s)
  member this.ReturnFrom(s: Coroutine<'a, 's>) = s
  member this.Bind(p: Coroutine<'a, 's>, k: 'a -> Coroutine<'b, 's>): Coroutine<'b, 's> =
    fun s ->
      match p s with
      | Done (x, s') -> k x s'
      | Yield (p', s') -> Yield(this.Bind(p', k), s')
let cs = CoroutineBuilder()

let printX a = fun s -> printfn "%A" a
                        Done((),s)

let rec repeat s =
  cs{
    do! s
    return! repeat s
  }

let yield_ = fun s -> Yield((fun s -> Done((),s)),s)

let wait interval =
  let time = fun s -> Done(System.DateTime.Now, s)
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
  | Done(a, s) -> (fun s -> (cs{return a}) s)
  | Yield(p', s) -> p'





//PROGRAM

let sample =
  cs{
    do! wait 3.0
    do! printX "pretty fucking printer1"
    do! wait 3.0
    do! printX "pretty fucking printer2"
    do! wait 3.0
    do! printX "pretty fucking printer3"
  }

let rec mainloop c =
  let x = co_step (c())
  mainloop x

do mainloop sample//THIS ACTUALLY WORKS