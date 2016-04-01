type Cor<'a, 's> = 's -> CorResult<'a, 's>
and CorResult<'a, 's> =
  | Done of 'a* 's
  | NotDone of Cor<'a, 's>*'s

let rec bind (p:Cor<'a, 's>) (k:'a -> Cor<'b, 's>) : Cor<'b, 's> =
  fun s ->
    match p s with
    | Done(a, s') -> k a s'
    | NotDone(cor', s') -> NotDone((bind cor' k), s')

type CoroutineBuilder() =
  member this.Bind(p,k) = bind p k
  member this.Return(a) = fun s -> Done(a, s)
  member this.ReturnFrom(c) = c
let cor = CoroutineBuilder()

let yield_ = fun s -> NotDone((fun s -> Done((), s)), s)

let rec repeat c =
  cor{
    do! c
    return! repeat c
  }

let wait x =
  let getCurrentTime = fun s -> Done(System.DateTime.Now, s)
  cor{
    let! beginTime = getCurrentTime
    let rec innerwait() =
      cor{
        let! currentTime = getCurrentTime
        let dt = (currentTime-beginTime).TotalSeconds
        if dt > x then
          return ()
        else
          do! yield_
          return! innerwait()
    }
    do! innerwait()
  }

let printX x =
  fun s ->
    printfn "%A" x
    Done((), s)

let rec (.&&) x y =
  fun s ->
    match x s, y s with
    | Done(_, sx'), Done(_, sy')          -> Done((), sx')
    | NotDone(x', sx'), Done(_, sy')      -> NotDone(((.&&) x' (fun s -> Done((), sy'))), sx')
    | NotDone(x', sx'), NotDone(y', sy')  -> NotDone(((.&&) x' y'), sx')
    | Done(_, sy'), _                     -> Done((), sy')
    
let A =
  fun s ->
    printfn "coroutine is waiting for stuff..."
    Done((), s)


let keepuserbusy() =
  cor{
    do! A
    do! wait 0.25
    do! yield_
    return ()
  }

let example() =
  cor{
    do! printX "coroutine has started"
    do! wait 1.0
    do! printX "three seconds have passed"
    do! wait 1.0
    do! printX "three more seconds have passed"
    do! wait 1.0
    do! printX "I have finished doing shit"
    return ()
  }

let mainCoroutine() =
  cor{
    do! (example()) .&& (repeat (keepuserbusy()))
    return ()
  }

let rec corresult c s =
  match c s with
  | Done(a, s') -> printfn "we zijn helemaal klaar"
                   ()
  | NotDone(c', s') -> corresult c' s'

corresult (mainCoroutine()) ()
