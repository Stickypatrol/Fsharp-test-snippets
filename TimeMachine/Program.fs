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

let rec mainloop c rs =
  let c', rs' = match Console.ReadLine() with
                | x when x = "f" -> match Console.ReadLine() with
                                    | x when Int32.Parse(x) > 0 -> ForwardFor(c, rs) (Int32.Parse(x))
                                    | _ -> c, rs
                | x when x = "b" -> match Console.ReadLine() with
                                    | x when Int32.Parse(x) > 0 -> c, BackwardFor rs (Int32.Parse(x))
                                    | _ -> c, BackwardFor rs 0
                | _ -> StepForward (c, rs)
  printf "%A\n" rs'.CurState
  mainloop c' rs'

do mainloop operatefunc { CurState      = state
                          StartState    = state
                          PastSteps     = [yield_]}