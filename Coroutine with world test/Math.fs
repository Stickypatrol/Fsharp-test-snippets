module Math

type Vector2 = {X : int; Y : int}
  with static member (+) (a, b) =
        {X = a.X + b.X; Y = a.Y + b.Y}