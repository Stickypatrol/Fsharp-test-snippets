module alt

type Coroutine<'a> = Unit -> CoroutineStep<'a>
and CoroutineStep<'a> =
  | Done of 'a
  | NotDone of Coroutine<'a>

let ret x = fun () -> Done(x)

let rec bind p k =
  fun () ->
    match p () with
    | Done res -> k res ()
    | NotDone p' -> NotDone(bind p' k)

type Builder() =
  member this.Return x = ret x
  member this.ReturnFrom a = a
  member this.Bind (p,k) = bind p k
let co = Builder()

let printA = fun () ->  printfn "print A has run"
                        Done ()

let printB = fun () ->  printfn "print B has run"
                        Done ()

let printC = fun () ->  printfn "print C has run"
                        Done ()

let yield_ = fun s -> NotDone(fun s -> Done())

let wait interval =
  let time = fun _ -> Done(System.DateTime.Now)
  printfn "started counting"
  co{
    let! t0 = time
    let rec wait() =
      co{
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

let rec repeat_ s =
  co{
    do! s
    return! repeat_ s
  }

let co_step =
  function
  | Done a -> co{return a}
  | NotDone c -> c

//ACTUAL PROGRAM

let sample =
  co{
    do! printA
    do! wait 3.0
    do! printB
    do! wait 3.0
    do! printC
    do! wait 3.0
    return ()
  }

let rec reccer coroutine =
  let nextcoroutine = co_step (coroutine())
  match nextcoroutine() with
  | Done x -> printfn "finished"
  | NotDone c -> ()
  do reccer nextcoroutine


let main() =
  do reccer sample