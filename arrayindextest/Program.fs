open System

let rec mainLoop i x (stopwatch:Diagnostics.Stopwatch) =
  if i > 0 then
    let floaty = float x
    let floaty' = floaty+1.0
    let inty = int floaty'
    mainLoop (i-1) inty stopwatch
  else
    printfn "Done in %A ms" stopwatch.ElapsedMilliseconds
    let num = Console.ReadLine()
    let stopwatch = Diagnostics.Stopwatch.StartNew()
    mainLoop (Int32.Parse(num)) 1 stopwatch


let stopwatch = Diagnostics.Stopwatch.StartNew()
do mainLoop 100000 3456 stopwatch
