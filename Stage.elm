module Stage 
  ( Stage
  , ForATime
  , Forever
  , forever
  , for
  , stayFor
  , stayForever
  , dilate
  , cycle
  , map
  , followedBy
  , chainTo
  , run
  ) where

{-| A type and functions on it for building up values as a function of time.

# Formation
@docs Stage, ForATime, Forever

# Introduction
@docs forever, for, stayFor, stayForever

# Transformation
@docs dilate, cycle, map

# Composition
@docs followedBy, chainTo

# Elimination
@docs run

-}

import Signal
import Time(Time)
import Time
import Debug

modFloat : Float -> Float -> Float
modFloat x m = x - m * toFloat (floor (x / m))

type Duration = ForATime Time | Forever

{-| A `Stage t a` is essentially a function `f : Time -> a` with
an associated duration `dur` which indicates that `f` is
meant to be considered as a function on the
time interval [0, dur].

The first type paramater `t` is `ForATime` if that duration
is some finite time and is `Forever` if `f` is to be considered
a total function.

As an example, we might have a value 
`circlePos : Stage ForATime (Float, Float)` indicating the position
of a circle in an animation.
-}
type Stage t a = Stage Duration (Time -> a)

type Forever = ForeverDummy
type ForATime = ForATimeDummy

{-| Create a total `Stage`, defined on all times. -}
forever : (Time -> a) -> Stage Forever a
forever f = Stage Forever f

{-| Create a `Stage` which runs for the specified duration -}
for : Time -> (Time -> a) -> Stage ForATime a
for dur f = Stage (ForATime dur) f

{-| A constant `Stage`.

    stayFor dur x = for dur (\_ -> x)
-}
stayFor : Time -> a -> Stage ForATime a
stayFor dur x = for dur (\_ -> x)

{-| An eternally constant `Stage` 

    stayForever x = forever (\_ -> x)
-}
stayForever : a -> Stage Forever a
stayForever x = forever (\_ -> x)


{-| Speed up or slow down time by the given factor. E.g.,

    dilate 0.5 s

is a `Stage` which lasts twice as long as `s` and proceeds half
as fast -}
dilate : Float -> Stage t a -> Stage t a
dilate s (Stage dur f) =
  let dur' =
    if s == 0
    then Forever
    else case dur of { ForATime t -> ForATime (t / s); _ -> dur }
  in Stage dur' (\t -> f (s * t))

{-| Repeat the given `Stage` forever. -}
cycle : Stage ForATime a -> Stage Forever a
cycle (Stage dur f) =
  case dur of
    ForATime d -> Stage Forever (\t -> f (modFloat t d))
    _          -> Debug.crash "The impossible happened: Stage.cycle"

map : (a -> b) -> Stage t a -> Stage t b
map g (Stage dur f) = Stage dur (g << f)

mkF d1 f1 f2 =
  \t -> if t <= d1 then f1 t else f2 (t - d1)

{-| Sequence two stages. Thinking of stages as functions of
time, s1 `followedBy` s2 is a piecewise function which acts like
`s1` for the duration of s1 and afterwards acts like `s2` (shifted
by the duration of `s1`). For example, we could write

    for (3 * second) (\_ -> "hi") `followedBy` for (1 * second) (\_ -> "bye")

which corresponds to a function of time which is `"hi"` for times in
the interval `[0, 3]` and `"bye"` in the interval `(3, 4]`. Consider 
using the synonym `(<>)` from `Stage.Infix`.
-}
followedBy : Stage ForATime a -> Stage t a -> Stage t a
followedBy (Stage dur1 f1) (Stage dur2 f2) =
  case (dur1, dur2) of
    (ForATime d1, Forever)     -> Stage Forever (mkF d1 f1 f2)
    (ForATime d1, ForATime d2) -> Stage (ForATime (d1 + d2)) (mkF d1 f1 f2)
    (Forever, _)               -> Debug.crash "The impossible happened: Stage.followedBy"

{-| Create a sequence of two stages, giving the second access to the
final value of the first. For example,

    pos = for (3 * second) (\t -> t * (100 / 1*second))
          `chainTo` \finalPos -> for (1*second) (\t -> finalPos - t * (finalPos / 1*second))

Consider using the synonym `(+>)` from `Stage.Infix`.
-}
chainTo : Stage ForATime a -> (a -> Stage t a) -> Stage t a
chainTo (Stage dur f) g =
  case dur of
    ForATime d -> Stage dur f `followedBy` g (f d)
    _          -> Debug.crash "The impossible happened: Stage.chainTo"

{-| Convert a Signal of `Stage`s into a `Signal` by sampling using the given
`Signal Time`. -}
run : Signal (Stage Forever a) -> Signal Time -> Signal a
run s ts = Signal.map2 (\(t0, Stage _ f) t -> f (t - t0)) (Time.timestamp s) ts

