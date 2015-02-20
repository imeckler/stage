module Stage
  ( Stage
  , ForATime
  , Forever
  , forever
  , for
  , stayFor
  , stayForever
  , sustain
  , dilate
  , cycle
  , map
  , followedBy
  , chainTo
  , run
  , finalValue
  , endToEnd
  ) where

{-| A type and functions on it for building up values as a function of time.

A `Stage t a` is essentially a function `f : Time -> a` with
an associated duration `dur` which indicates that `f` is
meant to be considered as a function on the
time interval [0, dur].

The first type paramater `t` is `ForATime` if that duration
is some finite time and is `Forever` if `f` is to be considered
a total function.

As an example, we might have a value 
`circlePos : Stage ForATime (Float, Float)` indicating the position
of a circle in an animation.

# Formation
@docs Stage, ForATime, Forever

# Introduction
@docs forever, for, stayFor, stayForever

# Transformation
@docs dilate, cycle, sustain, map

# Composition
@docs followedBy, chainTo

# Elimination
@docs finalValue, run

-}

import Stage.Internal as I
import Queue
import Signal
import Time(Time)
import Time
import Debug

modFloat : Float -> Float -> Float
modFloat x m = x - m * toFloat (floor (x / m))

type alias Stage t a = I.Stage t a

{-| A tag type indicating that a `Stage` is defined everywhere -}
type Forever = ForeverDummy
{-| A tag type indicating that a `Stage` is defined on some bounded interval. -}
type ForATime = ForATimeDummy

{-| Create a total `Stage`, defined on all times. -}
forever : (Time -> a) -> Stage Forever a
forever f = I.Stage I.Forever f

{-| Create a `Stage` which runs for the specified duration -}
for : Time -> (Time -> a) -> Stage ForATime a
for dur f = I.Stage (I.ForATime dur) f

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

{-| Get the last value the `Stage` takes on. -}
finalValue : Stage ForATime a -> a
finalValue (I.Stage (I.ForATime d) f) = f d

{-| Speed up or slow down time by the given factor. E.g.,

    dilate 0.5 s

is a `Stage` which lasts twice as long as `s` and proceeds half
as fast -}
dilate : Float -> Stage t a -> Stage t a
dilate s (I.Stage dur f) =
  let dur' =
    if s == 0
    then I.Forever
    else case dur of { I.ForATime t -> I.ForATime (t / s); _ -> dur }
  in I.Stage dur' (\t -> f (s * t))

{-| Repeat the given `Stage` forever. -}
cycle : Stage ForATime a -> Stage Forever a
cycle (I.Stage dur f) =
  case dur of
    I.ForATime d -> I.Stage I.Forever (\t -> f (modFloat t d))
    _            -> Debug.crash "The impossible happened: Stage.cycle"

map : (a -> b) -> Stage t a -> Stage t b
map g (I.Stage dur f) = I.Stage dur (g << f)

sustain : Stage ForATime a -> Stage Forever a
sustain st = st `followedBy` stayForever (finalValue st)

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
followedBy (I.Stage dur1 f1) (I.Stage dur2 f2) =
  case (dur1, dur2) of
    (I.ForATime d1, I.Forever)     -> I.Stage I.Forever (mkF d1 f1 f2)
    (I.ForATime d1, I.ForATime d2) -> I.Stage (I.ForATime (d1 + d2)) (mkF d1 f1 f2)
    (I.Forever, _)                 -> Debug.crash "The impossible happened: Stage.followedBy"

{-| Create a sequence of two stages, giving the second access to the
final value of the first. For example,

    pos = for (3 * second) (\t -> t * (100 / 1*second))
          `chainTo` \finalPos -> for (1*second) (\t -> finalPos - t * (finalPos / 1*second))

Consider using the synonym `(+>)` from `Stage.Infix`.
-}
chainTo : Stage ForATime a -> (a -> Stage t a) -> Stage t a
chainTo (I.Stage dur f) g =
  case dur of
    I.ForATime d -> I.Stage dur f `followedBy` g (f d)
    _          -> Debug.crash "The impossible happened: Stage.chainTo"

{-| Convert a Signal of `Stage`s into a `Signal` by sampling using the given
`Signal Time`. -}
run : Signal Time -> Signal (Stage Forever a) -> Signal a
run ts s =
  Signal.map2 (\(t0, I.Stage _ f) t -> f (t - t0)) (Signal.map2 (,) (Signal.sampleOn s ts) s) ts

type EntToEndUpdate a = CTime Time | CStage (Stage ForATime a)

{-| Create a signal by chaining together Stages end-to-end.
    The first argument (perhaps should be a -> Stage Forever a) -}

endToEnd : Stage Forever a -> Signal (Stage ForATime a) -> Signal Time -> Signal a
endToEnd (I.Stage _ gap) =
  let update u (x, t0, s, stages) =
        let (I.Stage (I.ForATime d) f) = s in
        case u of
          CTime t -> if t - t0 < d then (f t, t0, s, stages) else case Queue.pop stages of
            Nothing            -> (gap t, t0, s, Queue.empty)
            Just (s', stages') -> let (I.Stage _ g) = s' in (g t, t, s', stages')
          CStage s' -> (x, t0, s, Queue.push s' stages) -- should really filter out this event
  in
  \ss ts ->
    Signal.foldp update (gap 0, 0, for 0 gap, Queue.empty) {-dummy args-}
      (Signal.merge (Signal.map CStage ss) (Signal.map CTime ts))
    |> Signal.map (\(x,_,_,_) -> x)

