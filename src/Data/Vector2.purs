-- | - Types
-- |   - [Vec](#t:Vec)
-- |
-- | - Constructors
-- |   - [vec](#v:vec)
-- |   - [oneX](#v:oneX)
-- |   - [oneY](#v:oneY)
-- |
-- | - Destructors
-- |   - [unVec](#v:unVec)
-- |   - [getX](#v:getX)
-- |   - [getY](#v:getY)
-- |
-- | - Vector Modifiers
-- |   - [swap](#v:swap)
-- |
-- | - Component Modifiers
-- |   - [setX](#v:setX)
-- |   - [setY](#v:setY)
-- |   - [modifyX](#v:modifyX)
-- |   - [modifyY](#v:modifyY)
-- |
-- | - Lens API
-- |   - [_x](#v:_x)
-- |   - [_y](#v:_y)

module Data.Vector2
  ( Vec(..)
  , vec
  , oneX
  , oneY
  , unVec
  , getX
  , getY
  , swap
  , setX
  , setY
  , modifyX
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

data Vec a
  -- | Creates a vector from two components
  = Vec a a

derive instance Generic (Vec a) _

derive instance Eq a => Eq (Vec a)

derive instance Ord a => Ord (Vec a)

derive instance Functor Vec

instance Foldable Vec where
  foldr f b (Vec x y) = f x (f y b)
  foldl f b (Vec x y) = f (f b x) y
  foldMap = foldMapDefaultL

instance Traversable Vec where
  traverse f (Vec x y) = Vec <$> f x <*> f y
  sequence = sequenceDefault

instance Show a => Show (Vec a) where
  show = genericShow

-- | Componentwise `Semiring` implementation
-- | ```
-- | > Vec 2 3 * Vec 4 5
-- | Vec 8 15
-- | ```

instance Semiring a => Semiring (Vec a) where
  add = lift2 add
  zero = pure zero
  mul = lift2 mul
  one = pure one

instance Ring a => Ring (Vec a) where
  sub = lift2 sub

instance Applicative Vec where
  pure x = Vec x x

-- | Zippy `Apply` implementation
-- | ```
-- | > (<>) <$> Vec "A" "B" <*> Vec "1" "2"
-- | Vec "A1" "B2"
-- | ```

instance Apply Vec where
  apply (Vec f g) (Vec x y) = Vec (f x) (g y)

--------------------------------------------------------------------------------
--- Constructors
--------------------------------------------------------------------------------

-- | Creates a vector from two components
vec :: forall a. a -> a -> Vec a
vec = Vec

-- | Vector with X value `one` and Y value `zero`.
-- |
-- | In analogy to the existing `Semiring` methods `one` and `zero` for `Vec`
-- |
-- | ```
-- | > oneX + oneY == one
-- | true
-- | ```

oneX :: forall a. Semiring a => Vec a
oneX = Vec one zero

-- | Vector with X value `zero` and Y value `one`.
-- |
-- | In analogy to the existing `Semiring` methods `one` and `zero` for `Vec`.
-- |
-- | ```
-- | > oneX + oneY == one
-- | true
-- | ```

oneY :: forall a. Semiring a => Vec a
oneY = Vec zero one

--------------------------------------------------------------------------------
--- Destructors
--------------------------------------------------------------------------------

-- | Pattern match on a vector by providing a reducer function
-- |
-- | ```
-- | > unVec (+) (Vec 1 2)
-- | 3
-- | ```

unVec :: forall a z. (a -> a -> z) -> Vec a -> z
unVec f (Vec x y) = f x y

-- | Retrieves the X component of a vector
-- |
-- | ```
-- | > getX (Vec 1 2)
-- | 1
-- | ```

getX :: forall a. Vec a -> a
getX (Vec x _) = x

-- | Retrieves the Y component of a vector
-- |
-- | ```
-- | > getY (Vec 1 2)
-- | 2
-- | ```

getY :: forall a. Vec a -> a
getY (Vec _ y) = y

--------------------------------------------------------------------------------
--- Vector Modifiers
--------------------------------------------------------------------------------

-- | Exchanges the X and Y component of a vector
-- |
-- | ```
-- | > swap (Vec 1 2)
-- | Vec 2 1
-- | ```

swap :: forall a. Vec a -> Vec a
swap (Vec x y) = Vec y x

--------------------------------------------------------------------------------
--- Component Modifiers
--------------------------------------------------------------------------------

-- | Sets the X component of a vector
-- |
-- | ```
-- | > setX "C" (Vec "A" "B")
-- | Vec "C" "B"
-- | ```

setX :: forall a. a -> Vec a -> Vec a
setX x (Vec _ y) = Vec x y

-- | Sets the Y component of a vector
-- |
-- | ```
-- | > setY "C" (Vec "A" "B")
-- | Vec "A" "C"
-- | ```

setY :: forall a. a -> Vec a -> Vec a
setY y (Vec x _) = Vec x y

-- | Modifies the X component of a vector
-- |
-- | ```
-- | > modifyX (add 10) (Vec 3 4)
-- | Vec 13 4
-- | ```

modifyX :: forall a. (a -> a) -> Vec a -> Vec a
modifyX f (Vec x y) = Vec (f x) y

-- | Modifies the Y component of a vector
-- |
-- | ```
-- | > modifyY (add 10) (Vec 3 4)
-- | Vec 3 14
-- | ```

modifyY :: forall a. (a -> a) -> Vec a -> Vec a
modifyY f (Vec x y) = Vec x (f y)

--------------------------------------------------------------------------------
--- Lens API
--------------------------------------------------------------------------------

-- | A Lens on the X component of a vector

_x :: forall a. Lens' (Vec a) a
_x = lens getX (flip setX)

-- | A Lens on the Y component of a vector

_y :: forall a. Lens' (Vec a) a
_y = lens getY (flip setY)

