

let rand = System.Random()

type counter =
  {
    Min   : int
    Max   : int
    Avg   : float
    Count : int
  }

let rec MainLoop mapy i county =
  let A = (rand.Next System.Int32.MaxValue)
  let B = hash A
  if B < 0 then
    printfn "hash is %A" B
  else
    printfn "hash is positive"
  match Map.tryFind B mapy with
  | Some(x) ->  let min' = if i < county.Min then i else county.Min
                let max' = if i > county.Max then i else county.Max
                let avg' = ((county.Avg * (float county.Count)) + float i) / (float county.Count + 1.0)
                //printfn "FOUND A Match after %A iterations\naverage number of tries is: %A\nmin number of tries is: %A\nmax number of tries is: %A" i avg' min' max'
                //System.Threading.Thread.Sleep(100)
                MainLoop Map.empty 0 {Min = min'; Max = max'; Avg = avg'; Count = county.Count + 1}
  | None -> MainLoop (Map.add A B mapy) (i+1) county

do MainLoop Map.empty 0 ({Min = System.Int32.MaxValue; Max = 0; Avg = 0.0; Count = 0})