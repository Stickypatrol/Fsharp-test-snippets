open CoroutineMonad
open Drawing


let Player : Entity<'w, ActorFields, DrawContext> =
  {
    Fields = {Name = 'B'; Position = {X = 10;Y = 10}; Velocity = {X=0;Y=0}}
    Update = fun w fs ->
        Done((), {Name = fs.Name; Position = fs.Position + fs.Velocity; Velocity = fs.Velocity})
    Draw = fun (w, fs) dc -> Done((), dc)
  }




let rec mainloop c =
  let x = co_step (c() ())
  mainloop x

do mainloop (cs{return ()})