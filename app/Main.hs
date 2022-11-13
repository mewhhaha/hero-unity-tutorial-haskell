{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad (forever)
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.Monoid (Sum (Sum))
import Hero (Arrow ((&&&)), BoxedSparseSet, Global, Position2D (Position2D), System, TimeDelta (TimeDelta), Timer (..), addGlobal, addTimingComponents, cmap, cmap_, compileSystem, createEntity, createWorld, getGlobal, liftSystem, once, (>>>))
import Hero.Component (Component, Store)
import Hero.SDL2.Input (Scancode (ScancodeDown, ScancodeLeft, ScancodeRight, ScancodeUp), addSDLEvents, getKeyboardState, isPressed)
import Hero.SDL2.Render
  ( Render (Render, sprite),
    Shape (SCircle),
    Sprite (Shape, Texture),
    Texture,
    TextureSprite (TextureSprite, size, source, texture),
    defaultGraphics,
    fill,
    fillColor,
    loadTexture,
    offset,
    render,
    rotation,
    runGraphics,
    shape,
  )
import Linear.V2 (V2 (V2))
import Linear.V4 (V4 (V4))
import Optics.Core ((%), (&), (.~))

main :: IO ()
main = do
  world <- createWorld 1000
  run <- compileSystem system world
  forever $ do
    run ()

systemSetup :: System () ()
systemSetup = once $ do
  spawn ((Player, Position 0 0), Position2D 0 0, sprite)
  addGlobal (Input {right = (ScancodeRight, Up), left = (ScancodeLeft, Up), up = (ScancodeUp, Up), down = (ScancodeDown, Up)})
  pure ()

systemInput :: System () ()
systemInput =
  getGlobal @Input &&& getKeyboardState
    >>> cmap
      ( \(Input {right, left, up, down}, keyboardState) () ->
          let update (code, state) =
                let pressed = isPressed keyboardState code
                    next = case state of
                      Up -> if pressed then Pressed else Up
                      Down -> if pressed then Down else Released
                      Pressed -> if pressed then Down else Released
                      Released -> if pressed then Pressed else Up
                 in (code, next)
           in Input {right = update right, left = update left, up = update up, down = update down}
      )

tileSize :: Float
tileSize = 64

systemMovement :: System () ()
systemMovement = do
  getGlobal @Input
    >>> cmap
      ( \(Input {right, left, up, down}) (Position x y) ->
          let inputs = snd <$> [up, down, left, right]
              directions = [V2 0 1, V2 0 (-1), V2 (-1) 0, V2 1 0]
              states = (== Pressed) <$> inputs
              movement (pressed, dir) = Sum $ if pressed then dir else V2 0 0
              (Sum (V2 dx dy)) = foldMap movement $ states `zip` directions
           in Position (x + dx) (y + dy)
      )

  cmap_ (\(Position x y) -> Position2D (fromIntegral x * tileSize) (fromIntegral y * tileSize))
  pure ()

system :: System () ()
system =
  runGraphics defaultGraphics $ do
    addSDLEvents
    addTimingComponents

    systemSetup
    systemInput
    systemMovement

    pure ()

spawn e = pure e >>> createEntity

sprite :: Render
sprite =
  Render
    { rotation = 0,
      offset = 0,
      sprite =
        Shape
          { shape = SCircle 50,
            fill = fillColor $ V4 150 150 30 255
          }
    }

-- Game components

data Position = Position Int Int

instance Component Position where
  type Store Position = BoxedSparseSet

data Player = Player

instance Component Player where
  type Store Player = BoxedSparseSet

-- Input components

data ButtonState = Up | Down | Pressed | Released
  deriving (Eq)

data Input = Input
  { right :: (Scancode, ButtonState),
    left :: (Scancode, ButtonState),
    up :: (Scancode, ButtonState),
    down :: (Scancode, ButtonState)
  }

instance Component Input where
  type Store Input = Global