module PSignal 
  ( Duration
  , PSignal
  , forever
  , for
  , stayFor
  , stayForever
  , jumpTo
  , cycle
  , (>>>)
  , run
  , map
  , flattenSignal
  ) where

import Signal
import Time(Time)

import Native.PSignalUtil

modFloat : Float -> Float -> Float
modFloat = Native.PSignalUtil.modFloat

type Duration = Finite Time | Forever

-- The P is for Pure and also Partial.
type alias PSignal a = (Duration, Time -> a)

forever : (Time -> a) -> PSignal a
forever f = (Forever, f)

for : Time -> (Time -> a) -> PSignal a
for dur f = (Finite dur, f)

stayFor : Time -> a -> PSignal a
stayFor dur x = for dur (\_ -> x)

stayForever : a -> PSignal a
stayForever x = forever (\_ -> x)

jumpTo : a -> PSignal a
jumpTo x = stayFor 0 x

cycle : PSignal a -> PSignal a
cycle (dur, f) =
  case dur of
    Forever  -> (Forever, f)
    Finite d -> (Forever, \t -> f (modFloat t d))

mkF d1 f1 f2 =
  \t -> if t <= d1 then f1 t else f2 (t - d1)

(>>>) : PSignal a -> PSignal a -> PSignal a
(>>>) (dur1, f1) (dur2, f2) =
  case (dur1, dur2) of
    (Forever, _)           -> (Forever, f1)
    (Finite d1, Forever)   -> (Forever, mkF d1 f1 f2)
    (Finite d1, Finite d2) -> (Finite (d1 + d2), mkF d1 f1 f2)

run : PSignal a -> Time -> a
run (_, f) = f

map : (a -> b) -> PSignal a -> PSignal b
map g (dur, f) = (dur, g << f)

flattenSignal : Signal (PSignal a) -> Signal Time -> Signal a
flattenSignal = Signal.map2 run

