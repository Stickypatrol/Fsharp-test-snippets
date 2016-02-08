type Cstate<'a, 's> = 's -> CstateStep<'a, 's>
and CstateStep<'a, 's> =
  | Done of 'a * 's
  | ArrowYield of Cstate<'a, 's>* 's
  | Yield of Cstate<'a, 's>* 's

let ret a = fun s -> Done(a, s)

let rec bind p k =
  fun s ->
    match p s with
    | Done(a, s') -> k a s'
    | ArrowYield(p', s') -> Yield(bind p' k, s')
    | Yield(p', s') -> Yield(bind p' k, s')

let (>>=) = bind

type CstateBuilder() =
  member this.Return x = ret x
  member this.ReturnFrom x = x
  member this.Bind(p,k) = bind p k
  member this.Zero() = this.Return ()
let cs = CstateBuilder()

let yield_ = fun s -> Yield((fun s -> Done((),s)),s)

let wait interval =
  let time = fun s -> Done(System.DateTime.Now, s)
  cs{
    let! t0 = time
    let rec wait_ () =
      cs{
        let! t = time
        let dt = (t-t0).TotalSeconds
        if dt < interval then
          return! wait_ ()
      }
    do! wait_ ()
  }

let rec concur c1 c2 =
  fun (s:'s) ->
    match c1 s, c2 s with
    | Done(a, s'), _ -> Done(a, s')
    | _, Done(a, s') -> Done(a, s')
    | ArrowYield(c1', s1'), _ -> Yield(cs{let! res = c1'
                                          return res}, s1')
    | _, ArrowYield(c1', s1') -> Yield(cs{let! res = c1'
                                          return res}, s1')
    | Yield(c1', s1'), Yield(c2', s2') -> Yield((concur c1' c2'), s1')

let rec guard c f =
  cs{
    let! x = c
    if x then
      let! res = f
      return res
    else
      return! guard c f
  }



//ACTUAL PROGRAM
let accounts = [("bob", "pw")
                ("sjors", "hurrdurr")]



let checkfunc input =
  fun (s:Map<string, string>) ->
    match s.TryFind input with
    | Some x -> (cs{return Some(x)}) s
    | None -> (cs{return None}) s

let inputfunc =
  fun s ->
    Done(System.Console.ReadLine(), s)

(*let rec mainloop() =
  cs{
    let! input = inputfunc
    let! hit = checkfunc input
    let! result = 
  }*)