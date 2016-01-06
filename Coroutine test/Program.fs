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

let rec repeat_ s =
  co{
    do! s
    return! repeat_ s
  }


//ACTUAL PROGRAM

let rec mainloop() =
  co{
    do! repeat_ printA
    return ()
  }
  do mainloop()

do mainloop()