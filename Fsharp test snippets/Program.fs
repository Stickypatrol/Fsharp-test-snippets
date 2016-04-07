type Coroutine<'a, 's> = 's -> CoroutineResult<'a, 's>
and CoroutineResult<'a, 's> =
  | Done of 'a*'s
  | NotDone of Coroutine<'a, 's>*'s

let ret x = fun s -> Done(x, s)

let rec bind p k =
  fun s ->
    match p s with
    | Done(a, s') -> k a s'
    | NotDone(c', s') -> NotDone((bind c' k), s')

type MonadBuilder() =
  member this.Return(x) = ret x
  member this.ReturnFrom(c) = c
  member this.Bind(p,k) = bind p k
let cor = MonadBuilder()

let print_ x =
  fun s ->
    printfn "%A" x
    Done((), s)

let rec repeat_ c =
  cor{
    do! c
    return! repeat_ c
  }

let yield_ = fun s -> NotDone((fun s -> Done((), s)), s)

let wait_ interval =
  let getTime : 's -> CoroutineResult<System.DateTime, 's> = fun s -> Done(System.DateTime.Now,s)
  cor{
    let! starttime = getTime
    let rec wait() : 's -> CoroutineResult<Unit, 's> =
      cor{
        let! currenttime = getTime
        let passedtime = (currenttime - starttime).TotalSeconds
        if passedtime > interval then
          return ()
        else
          do! yield_
          return! wait()
      }
    do! wait()
    return ()
  }

let rec (||.) p k =
  fun s ->
    match p s, k s with
    | Done(_, s'), Done(_, _) -> Done((), s')
    | NotDone(ca', sa'), NotDone(cb', sb') -> NotDone(((||.) ca' cb'), sa')
    | NotDone(ca', sa'), Done(_, sb') -> NotDone(((||.) ca' (fun s -> Done((), s))), sa')
    | Done(_, s'), NotDone(cb', sb') -> Done((), s')

let functionA () =
  cor{
    do! print_ "starting lesson and important stuff"
    do! wait_ 3.0
    do! print_ "stopping lesson to listen to unimportant yelling by people who dont do shit in this classroom"
    do! wait_ 3.0
    do! print_ "finished lesson"
    return ()
  }

let functionB () =
  cor{
    do! print_ "BLABLABLA"
    do! wait_ 0.1
  } |> repeat_

let completeFunction() =
  cor{
    do! (functionA()) ||. (functionB())
    return ()
  }


let rec costep coroutine state =
  match coroutine state with
  | Done(_, newState) ->  printfn "WE ARE ALL DONE WITH THESE FUNCTIONS!!!!!!"
                          (fun s -> Done((), s)), newState
  | NotDone(c', s') -> costep c' s'



let _, finalstate = costep (completeFunction()) ()