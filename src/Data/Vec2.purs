-- | - Types
-- |   - [Vec2](#t:Vec2)
-- |
-- | - Constructors
-- |   - [vec2](#v:vec2)
-- |   - [oneY](#v:oneY)
-- |   - [oneX](#v:oneX)
-- |
-- | - Destructors
-- |   - [unVec2](#v:unVec2)
-- |   - [getX](#v:getX)
-- |   - [getY](#v:getY)
-- |
-- | - Vector Modifiers
-- |   - [swap](#v:swap)
-- |
-- | - Component Modifiers
-- |   - [setX](#v:setX)
-- |   - [modifyX](#v:modifyX)
-- |   - [setY](#v:setY)
-- |   - [modifyY](#v:modifyY)
-- |
-- | - Lens API
-- |   - [_x](#v:_x)
-- |   - [_y](#v:_y)

module Data.Vec2
  ( Vec2(..)
  , vec2
  , oneX
  , oneY
  , unVec2
  , getX
  , getY
  , swap
  , setX
  , modifyX
  , setY
  , modifyY
  , _x
  , _y
  ) where

import Prelude

import Control.Apply (lift2)
import Data.Foldable (class Foldable, foldMapDefaultL)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', lens)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, sequenceDefault)

--------------------------------------------------------------------------------
--- Types
--------------------------------------------------------------------------------

-- | Polymorphic 2D vector

data Vec2 a
  -- | Creates a vector from two components
  = Vec2 a a

derive instance Generic (Vec2 a) _

derive instance Eq a => Eq (Vec2 a)

derive instance Ord a => Ord (Vec2 a)

derive instance Functor Vec2

instance Foldable Vec2 where
  foldr f b (Vec2 x y) = f x (f y b)
  foldl f b (Vec2 x y) = f (f b x) y
  foldMap = foldMapDefaultL

instance Traversable Vec2 where
  traverse f (Vec2 x y) = Vec2 <$> f x <*> f y
  sequence = sequenceDefault

instance Show a => Show (Vec2 a) where
  show = genericShow

-- | component wise `Semiring` implementation
-- | ```
-- | > Vec2 2 3 * Vec2 4 5
-- | Vec2 8 15
-- | ```

instance Semiring a => Semiring (Vec2 a) where
  add = lift2 add
  zero = pure zero
  mul = lift2 mul
  one = pure one

instance Ring a => Ring (Vec2 a) where
  sub = lift2 sub

instance Applicative Vec2 where
  pure x = Vec2 x x

-- | Zippy `Apply` implementation
-- | ```
-- | > (<>) <$> Vec2 "A" "B" <*> Vec2 "1" "2"
-- | Vec2 "A1" "B2"
-- | ```

instance Apply Vec2 where
  apply (Vec2 f g) (Vec2 x y) = Vec2 (f x) (g y)

--------------------------------------------------------------------------------
--- Constructors
--------------------------------------------------------------------------------

-- | Creates a vector from two components
vec2 :: forall a. a -> a -> Vec2 a
vec2 = Vec2

-- | Vector with X value `zero` and Y value `one`.
-- |
-- | In analogy to the existing `Semiring` methods `one` and `zero` for `Vec2`.
-- |
-- | ```
-- | > oneX + oneY == one
-- | true
-- | ```

oneY :: forall a. Semiring a => Vec2 a
oneY = Vec2 zero one

-- | Vector with X value `one` and Y value `zero`.
-- |
-- | In analogy to the existing `Semiring` methods `one` and `zero` for `Vec2`
-- |
-- | ```
-- | > oneX + oneY == one
-- | true
-- | ```

oneX :: forall a. Semiring a => Vec2 a
oneX = Vec2 one zero

--------------------------------------------------------------------------------
--- Destructors
--------------------------------------------------------------------------------

-- | Pattern match on a vector by providing a reducer function
-- |
-- | ```
-- | > unVec2 (+) (Vec 1 2)
-- | 3
-- | ```

unVec2 :: forall a z. (a -> a -> z) -> Vec2 a -> z
unVec2 f (Vec2 x y) = f x y

-- | Retrieves the X component of a vector
-- |
-- | ```
-- | > getX (Vec 1 2)
-- | 1
-- | ```

getX :: forall a. Vec2 a -> a
getX (Vec2 x _) = x

-- | Retrieves the Y component of a vector
-- |
-- | ```
-- | > getY (Vec 1 2)
-- | 2
-- | ```

getY :: forall a. Vec2 a -> a
getY (Vec2 _ y) = y

--------------------------------------------------------------------------------
--- Vector Modifiers
--------------------------------------------------------------------------------

-- | Exchanges the X and Y component of a vector
-- |
-- | ```
-- | > swap (Vec2 1 2)
-- | Vec2 2 1
-- | ```

swap :: forall a. Vec2 a -> Vec2 a
swap (Vec2 x y) = Vec2 y x

--------------------------------------------------------------------------------
--- Component Modifiers
--------------------------------------------------------------------------------

-- | Sets the X component of a vector
-- |
-- | ```
-- | > setX "C" (Vec2 "A" "B")
-- | Vec2 "C" "B"
-- | ```

setX :: forall a. a -> Vec2 a -> Vec2 a
setX x (Vec2 _ y) = Vec2 x y

modifyX :: forall a. (a -> a) -> Vec2 a -> Vec2 a
modifyX f (Vec2 x y) = Vec2 (f x) y

setY :: forall a. a -> Vec2 a -> Vec2 a
setY y (Vec2 x _) = Vec2 x y

modifyY :: forall a. (a -> a) -> Vec2 a -> Vec2 a
modifyY f (Vec2 x y) = Vec2 x (f y)

--------------------------------------------------------------------------------
--- Lens API
--------------------------------------------------------------------------------

_x :: forall a. Lens' (Vec2 a) a
_x = lens getX (flip setX)

_y :: forall a. Lens' (Vec2 a) a
_y = lens getY (flip setY)

