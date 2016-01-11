type Maybe<'a> =
  | None
  | Some of 'a
  with
  static member ret a = Some(a)
  static member bind p k =  match p with
                            | Some x -> k x
                            | None -> None

type MonadType<'a> =
  | Maybe of Maybe<'a>

type Wrapper<'a> = Mtype of 'a

type GenericMonad<'a, 'b> =
  {
    ret : 'a -> Wrapper<'a>
    bind : (Wrapper<'a>* ('a -> Wrapper<'b>)) -> Wrapper<'b>
  }with
(*  static member CreateFromMonad (wrapper : MonadType<'a>) =
    match wrapper with
    | Maybe maybe ->{
                      ret = fun value -> value |> Mtype
                      bind = fun (p, k) -> Maybe.bind p k
                    }//DOESNT WORK, STOP TRYING*)