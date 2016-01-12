module Actors

open Math
open CoroutineMonad

type PlayerFields=
  {
    Name      : char
    Position  : Vector2
    Velocity  : Vector2
  }
  with static member update =
        fun (player : PlayerFields) -> player
  
type EnemyFields=
  {
    Name      : char
    Position  : Vector2
    Velocity  : Vector2
  }