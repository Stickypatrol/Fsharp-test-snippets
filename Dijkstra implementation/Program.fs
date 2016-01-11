open System
printfn "nothing will happen"
(*let testcases = Console.ReadLine()
let rec dijkstra results i =
  if i > 0 then
    let NandM = Console.ReadLine().Split()
    let N, M = NandM.[0], NandM.[1]

    let rec reader M (graphmap:Map<int, Map<int, int>>) : Map<int, Map<int, int>> =
      if M > 0 then
        let inputline = Console.ReadLine().Split()
        let A, B, C = Int32.Parse(inputline.[0]), Int32.Parse(inputline.[1]), Int32.Parse(inputline.[2])
        if A <> B then
            match (Map.tryFind A graphmap) with
            | None ->   let graphmap' = Map.add B (Map.add A C Map.empty<int, int>) graphmap
                        reader (M-1) (Map.add A (Map.add B C Map.empty<int, int>) graphmap')
            | Some x -> let graphmap' = Map.add B (Map.add A C x) graphmap
                        reader (M-1) (Map.add A (Map.add B C x) graphmap')
        else reader (M-1) graphmap
      else
        graphmap

    let graphmap = reader M (Map.empty<int, Map<int, int>>)
    
    let rec checker graph paths curr visited results =
      



    let root = Int32.Parse(Console.ReadLine())
    let curr = root
    let visited = Map.add root true Map.empty<int, bool>
    let rec checker graphmap paths results : List<int> =
      //3. assign tentative distance
      
      //4. mark curr as visited
      //5. check for completion*)