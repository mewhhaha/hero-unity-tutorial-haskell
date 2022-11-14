{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad (forever)
import Control.Monad.State (MonadIO (liftIO), MonadState (put), State, evalState, get)
import Data.Foldable (for_, sequenceA_, traverse_)
import Data.Function (on)
import Data.Functor (($>))
import Data.List (sortBy)
import Data.Maybe (catMaybes)
import Data.Monoid (Sum (Sum))
import Data.Set (Set, elemAt, fromList, member, size)
import Hero (Arrow ((&&&)), BoxedSparseSet, Entity, Global, Position2D (Position2D), QCP, Rectangle (Rectangle), System, TimeDelta (TimeDelta), Timer (..), addGlobal, addTimingComponents, cmap, cmap_, compileSystem, createEntity, createWorld, getGlobal, liftSystem, once, putGlobal, (>>>))
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
import Hero.SparseSet.Boxed (create)
import Hero.System (QueryPut (queryPut), S, System (System))
import Hero.World qualified as World
import Linear.V2 (V2 (V2))
import Linear.V4 (V4 (V4))
import Optics.Core ((%), (&), (.~))
import System.Random (Random (randoms), RandomGen, mkStdGen, newStdGen, randomR, randomRs, setStdGen, split)

tileSize :: Float
tileSize = 32

main :: IO ()
main = do
  world <- createWorld 1000
  run <- compileSystem system world
  forever $ do
    run ()

systemSetup :: System () ()
systemSetup = once $ do
  addGlobal (SpriteSheet Nothing)
  loadTexture "./assets/sprites/Scavengers_SpriteSheet.png" >>> liftSystem (pure . SpriteSheet . Just) >>> putGlobal
  addGlobal (Input {right = (ScancodeRight, Up), left = (ScancodeLeft, Up), up = (ScancodeUp, Up), down = (ScancodeDown, Up)})

  createLevel 0 100

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

systemMovement :: System () ()
systemMovement = do
  getGlobal @Input
    >>> cmap
      ( \(Input {right, left, up, down}) (Position x y, Player) ->
          let inputs = snd <$> [up, down, left, right]
              directions = [V2 0 (-1), V2 0 1, V2 (-1) 0, V2 1 0]
              states = (== Pressed) <$> inputs
              movement (pressed, dir) = if pressed then dir else V2 0 0
              (Sum (V2 dx dy)) = foldMap (Sum . movement) $ states `zip` directions
           in Position (x + dx) (y + dy)
      )

  pure ()

nRows :: Float
nRows = 14

nColumns :: Float
nColumns = 19

widthOffset :: Float
widthOffset = (nColumns * tileSize) / 2

heightOffset :: Float
heightOffset = (nRows * tileSize) / 2

systemRender :: System () ()
systemRender = do
  cmap_ (\(Position x y) -> Position2D (fromIntegral x * tileSize - widthOffset) ((nRows - fromIntegral y) * tileSize - heightOffset))

system :: System () ()
system =
  runGraphics defaultGraphics $ do
    addSDLEvents
    addTimingComponents

    systemSetup
    systemInput
    systemMovement
    systemRender

    pure ()

clip :: V2 Float -> Texture -> Render
clip tile texture =
  Render
    { rotation = 0,
      offset = 0,
      sprite =
        Texture $
          TextureSprite
            { size = V2 tileSize tileSize,
              source = Just (Rectangle (tile * pure tileSize) (V2 tileSize tileSize)),
              texture
            }
    }

-- Level generation

newLevel :: Int -> System () ()
newLevel l = cmap_ $ \() -> Level l

rows :: [Int]
rows = [1 .. floor nRows - 1]

columns :: [Int]
columns = [1 .. floor nColumns - 1]

start :: (Int, Int)
start = (1, last rows)

goal :: (Int, Int)
goal = (last columns, 1)

createLevel :: Int -> Int -> System () ()
createLevel l life = do
  spawn $ \s -> ((Player, uncurry Position start), clip (V2 0 0) s)
  spawn $ \s -> ((End, uncurry Position goal), clip (V2 4 2) s)

  spawnAll zombies $ \pos s -> ((Zombie, uncurry Position pos), clip (V2 6 0) s)
  spawnAll vampires $ \pos s -> ((Vampire, uncurry Position pos), clip (V2 4 1) s)
  spawnAll sodas $ \pos s -> ((Soda, uncurry Position pos), clip (V2 2 2) s)
  spawnAll fruits $ \pos s -> ((Fruit, uncurry Position pos), clip (V2 3 2) s)
  spawnAll obstacles $ \pos s -> ((Obstacle, uncurry Position pos), clip (V2 0 3) s)
  spawnAll edges $ \pos s -> (uncurry Position pos, clip (V2 1 3) s)

  pure ()
  where
    spawn f = getGlobal @SpriteSheet >>> liftSystem (\(SpriteSheet (Just t)) -> pure (f t)) >>> createEntity
    spawnAll es f = traverse_ (spawn . f) es
    g = mkStdGen l
    area = [(x, y) | x <- columns, y <- rows, (x, y) `notElem` [start, goal]]
    [zombies, vampires, sodas, fruits, obstacles] =
      flip evalState area $
        sequence [pick g 5, pick g 3, pick g 3, pick g 3, pick g 10]

-- Board generation

edges :: Set (Int, Int)
edges = fromList (xs (,0) ++ xs (,floor nRows) ++ ys (0,) ++ ys (floor nColumns,))
  where
    xs f = f <$> [0 .. floor nColumns]
    ys f = f <$> [0 .. floor nRows]

shuffle :: RandomGen r => r -> [a] -> [a]
shuffle r = fmap snd . sortBy (compare `on` fst) . zip (randoms @Int r)

pick :: RandomGen r => r -> Int -> State [a] [a]
pick r n = do
  state <- get
  let (picked, rest) = splitAt n (shuffle r state)
  put rest
  return picked

-- Game components

data Position = Position Int Int

instance Component Position where
  type Store Position = BoxedSparseSet

data Player = Player

instance Component Player where
  type Store Player = BoxedSparseSet

data End = End

instance Component End where
  type Store End = BoxedSparseSet

data Obstacle = Obstacle

instance Component Obstacle where
  type Store Obstacle = BoxedSparseSet

data Pickup = Soda | Fruit

instance Component Pickup where
  type Store Pickup = BoxedSparseSet

data Enemy = Zombie | Vampire

instance Component Enemy where
  type Store Enemy = BoxedSparseSet

-- Global components

newtype SpriteSheet = SpriteSheet (Maybe Texture)

instance Component SpriteSheet where
  type Store SpriteSheet = Global

newtype Level = Level Int

instance Component Level where
  type Store Level = Global

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
