module Drawing

open Microsoft.Xna.Framework.Graphics

type DrawContext =
  {
    SpriteBatch : SpriteBatch
    Player      : Texture2D
    Asteroid    : Texture2D
  }