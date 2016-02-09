﻿module CoroutineMonad

//MONAD
type Coroutine<'a, 's> = 's -> CoroutineStep<'a, 's>
and CoroutineStep<'a, 's> =
  | Done of 'a*'s
  | Pause of Coroutine<'a, 's>*'s

type RewindState<'a, 's> =
  {
    CurState    : 's
    StartState  : 's
    PastSteps   : List<'s -> CoroutineStep<'a,'s>>
  }

//MONAD IMPLEMENTATION
let ret x = fun s -> Done(x, s)

let rec bind p k =
  fun s ->
    match p s with
    | Done(a, s') -> k a s'
    | Pause(p', s') -> Pause(bind p' k, s')

type CoroutineBuilder() =
  member this.Return x = ret x
  member this.ReturnFrom (c:Coroutine<'a, 's>) = c
  member this.Bind(p,k) = bind p k
let co = CoroutineBuilder()

//MONADIC BASE FUNCTIONS

let yield_ = fun s -> Pause((fun s -> Done((), s)), s)

let rec repeat s =
  co{
    do! s
    do! yield_
    return! repeat s
  }

let wait interval =
  let time = fun s -> Done(System.DateTime.Now, s)
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
    do! wait ()
  }
  
let costep c =
  match c with
  | Done(a, s') -> (fun s -> Done(a, s)), s'
  | Pause(c', s') -> c', s'

//ACTUALLY INTERESTING FUNCTIONS

let rec Rewind pastSteps x =
  match pastSteps, x with
  | h::t, x when x <> 0 -> Rewind t (x-1)
  | h::t, x when x = 0 -> List.rev t
  | _ -> []

let StepForward (c:Coroutine<'a, 's>, rs:RewindState<'a, 's>) =
  let c', s' = costep (c rs.CurState)
  let rs' = {rs with  CurState = s'
                      PastSteps = (c::rs.PastSteps)}
  c', rs'

let rec Rerun (c:List<Coroutine<Unit, 's>>, rs:RewindState<Unit, 's>) =
  match c with
  | h::t when t.Length > 0 -> let c', rs' = StepForward (h, rs)
                              Rerun (t, rs')
  | h::t -> StepForward (h, rs)
  | [] -> yield_, rs
  
let MultiStepBackward rs x =
  snd(Rerun((Rewind rs.PastSteps x), {rs with CurState =  rs.StartState
                                                          PastSteps = [co{return ()}]}))

let rec BackwardFor rs x =
  if x > 0 then
    MultiStepBackward rs x
  else
    rs

let rec ForwardFor (c, rs) x =
  if x > 0 then
    ForwardFor (StepForward (c, rs)) (x-1)
  else
    c, rs