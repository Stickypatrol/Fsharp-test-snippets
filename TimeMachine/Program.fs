open CoroutineMonad
open System
//AUXILIARY FUNCTIONS TO ACTUALLY DO SOMETHING USEFUL WITH THE ABOVE FUNCTIONS

let state = 1
let countfunction = fun x -> Done((),x+1)

let operatefunc : Coroutine<Unit, int> =
  co{
    do! countfunction
    return ()
  } |> repeat

let x = RewindOperator operatefunc {CurState    = state
                                    StartState  = state
                                    PastSteps   = [yield_]}
printfn "%A" x