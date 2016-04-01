type Data =
  | Float of float
  | Int of int
  | String of string
  | Tuple of Data*Data
  | Union of Choice<Data,Data> //ADD THIS TO THE GAME LATER DON'T USE IT YET, IT WORKS WITH THE TO DATA METHOD
  with
  static member Unwrap data : Data list =
      match data with
      | Float(x) -> Float(x)::[]
      | Int(x) -> Int(x)::[]
      | String(x) -> String(x)::[]
      | Tuple(x, y) -> (Data.Unwrap x)@(Data.Unwrap y)
      | Union(x) -> match x with
                    | Choice1Of2(x) -> Data.Unwrap x
                    | Choice2Of2(x) -> Data.Unwrap x

let a = Int(2)

let b = Tuple(String("awda"), Float(5.78))

let c = Tuple(Tuple(String("awda"), Float(5.78)), Tuple(Int(23768), Float(0.23456)))

let f = Data.Unwrap c
let d = Data.Unwrap a
let e = Data.Unwrap b


printfn "%A\n%A\n%A\n%A\n%A\n%A\n" a b c d e f
