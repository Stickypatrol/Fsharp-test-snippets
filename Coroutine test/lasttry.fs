module lasttry

type Cstate<'a,'s> = 's -> CstateStep<'a,'s>
and CstateStep<'a,'s> =
  | Done of 'a* 's
  | Yield of Cstate<'a, 's>* 's


let ret x = fun s -> Done(x,s)

let bind p k =
  fun s ->
    match p s with
    | Done(a, s') -> k a s'
    | Yield(p', s') -> Yield((fun s -> Done((), s')),s')

type CBuilder() =
  member this.Return x = ret x
  member this.ReturnFrom a = a
  member this.Bind (p,k) = bind p k
let cs = CBuilder()

let printA = fun s -> printfn "print A has run"
                      Done ((),s)

let printB = fun s -> printfn "print B has run"
                      Done ((),s)

let printC = fun s -> printfn "print C has run"
                      Done ((),s)

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
        printfn "%A" (interval - dt)
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

//ACTUAL PROGRAM

let csloop() : Cstate<Unit, 's> =
  cs{
    do! wait 1.0
    do! wait 3.0
    do! wait 5.0
    return ()
  }

let rec loopyloop() =
  cs{
    let! x = csloop()
    return! loopyloop()
  }


(*let rec loop (c:Cstate<Unit, string>) =
  let x = c <| "AWdawd"
  match co_step x with
  | Done(a, s) -> (cs{ return printfn "finished with %A" a}) s
  | Yield()

  do loop x

do loop loopyloop()*)