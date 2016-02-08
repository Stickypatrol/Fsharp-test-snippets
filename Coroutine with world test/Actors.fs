module Actors

open Microsoft.Xna.Framework.Graphics
open Math
open CoroutineMonad
open Drawing

type Entity<'w, 'fs, 'dc> =
  {
    Fields            : 'fs
    Update            : Coroutine<'w, 'fs, Unit>
    Draw              : Coroutine<'w* 'fs, 'dc, Unit>
  }

type ShipAttr =
  {
    Name      : char
    Position  : Vector2
    Velocity  : Vector2
  }

type EnemyAttr =
  {
    Target    : Option<char>
    Position  : Vector2
    Velocity  : Vector2
  }

type GameWorld =
  {
    Players : List<Entity<GameWorld, ShipAttr, DrawContext>>
    Enemies : List<Entity<GameWorld, EnemyAttr, DrawContext>>
    SpriteBatch : SpriteBatch
  }