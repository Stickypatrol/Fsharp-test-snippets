module CoroutineMonad

(*
this is a compound monad, it combines a coroutine monad, a state monad, a counting monad, and an N-dimensional combination monad
to be able to properly deal with concurrent computations, it keeps track of which concurrent operation it is at and updates the counter accordingly.
when it encounters a concurrent operation then it iterates the value which keeps track of which it is at and
adds a dimensional layer of states and unfinished coroutines on top of the already present ones.
when it encounters a Done(result, newstate) in a certain concurrent operation then it 'trims' the dimensionality of the N-dimensional and ditches
layers of states that are not needed anymore.
*)

type Coroutine<'w, 's, 'a> = int*'w -> Map<List<int>, 's> -> int*Map<List<int>, CoroutineStep<'w, 's, 'a>>
and CoroutineStep<'w, 's, 'a> =
  | Done of 'a*'s
  | Yield of Coroutine<'w, 's, 'a>*'s

//let ret x : Coroutine<'w, 's, 'a> =
//  fun (i, w) s ->
//    (i, Done((),s))
//  fun w s -> Done(x, s)

(*let bind (p:Coroutine<'w, 's, 'a>) (k:'a -> Coroutine<'w, 's, 'b>) : Coroutine<'w, 's, 'b> =
  fun w sx ->
    List.fold (fun state c -> state)  cx
    //fuck it I cant do this shit
    
    
    we want to unwrap sx(producing a list of states)
    we then check for done or yield
    if done, call bind with the done state, returning a yield eventually
    these yields are put into a list and returned
    *)
