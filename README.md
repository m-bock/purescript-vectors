# purescript-vectors

Fixed size polymorphic vectors

## Example

In a nutshell:

```hs
import Data.Vector2 (Vec(..))

va :: Vec Int
va = Vec 2 3

vb :: Vec Int
vb = Vec 7 6

vc :: Vec Int
vc = va + vb
```


## Design decisions

### Fixed sizes
Commonly used Vectors are defined in separate fixed sized types. Currently for (2D and 3D vectors)

If you need an implementation that is polymorphic on the size of the vectors, you can check out the [fast-vect](https://pursuit.purescript.org/packages/purescript-fast-vect) package.

### Polymorphic

Vectors in this library are polymorphic on the inner type. Thus you can use it as `Vec Int` `Vec Number` or any other type. 

```hs
newtype PosInt = PosInt Int

type V = Vec PosInt
```

Even though there can also be something like `Vec String` most operations defined in here are constrainted to types that define numeric instances such as `Ring` or `Semiring`.

There may be more specific types provided by other libraries. E.g. a `VecNumber` may provide operations which are `Number` specific. Moreover it could be implemented as JavaScript's `Float64Array` for better interoperablity with WebGL or similar. This is for instance how it's handled in Elm's [linear-algebra](https://github.com/elm-explorations/linear-algebra/blob/master/src/Elm/Kernel/MJS.js#L65) library.

### Just Vectors

The library only provide data types and functions for vectors of different sizes, as well as some conversions between them. There will be no matrices or other geometric shapes included in this library. So it will stay a lightweight dependency.

### Naming

In many Programs either only 3D Vectors or only 2D Vectors are used. Thus all of the the vector data types are named equally. 

```
import Data.Vector2 (Vec)
import Data.Vector2 as Vec

f :: Vec -> Vec -> Int
f (Vec x y) ...
```

If you need more than one in a module you can still import them qualified.

```hs
import Data.Vector2 as V2
import Data.Vector3 as V3

f :: V2.Vec -> V3.Vec -> Int
f (V2.Vec x y) ...
```

`Vec` was chosen as it's somewhere in the middle of "descriptive" and "short". Other options were `Vector`, `Vect`, `V`. Note that we could only provide a short infix alternative for the 2D vector, so this was't provided at all for consistency reasons.


### Exposed Constructor

Implementation is exposed. This allows easy and clean destructuring of vector components. However it does not allow to change the implementation behind the scenes to something that is maybe more performant. Clarity and convenience was a higher priority than optimization. However with a contemporary [code optimizer](https://github.com/aristanetworks/purescript-backend-optimizer) this tradeoff is probably vastly reduced.

