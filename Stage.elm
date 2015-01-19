module Stage 
  ( Duration
  , Stage
  , forever
  , for
  , stayFor
  , stayForever
  , jumpTo
  , cycle
  , followedBy
  , chainTo
  , run
  , map
  , dilate
  , ForATime
  , Forever
  ) where

import Signal
import Time(Time)
import Time
import Debug
import Native.StageUtil

modFloat : Float -> Float -> Float
modFloat = Native.StageUtil.modFloat

type Duration = ForATime Time | Forever

type Stage t a = Stage Duration (Time -> a)

type Forever = ForeverDummy
type ForATime = ForATimeDummy

forever : (Time -> a) -> Stage Forever a
forever f = Stage Forever f

for : Time -> (Time -> a) -> Stage ForATime a
for dur f = Stage (ForATime dur) f

stayFor : Time -> a -> Stage ForATime a
stayFor dur x = for dur (\_ -> x)

stayForever : a -> Stage Forever a
stayForever x = forever (\_ -> x)

jumpTo : a -> Stage ForATime a
jumpTo x = stayFor 0 x

dilate : Float -> Stage t a -> Stage t a
dilate s (Stage dur f) =
  let dur' =
    if s == 0
    then Forever
    else case dur of { ForATime t -> ForATime (t / s); _ -> dur }
  in Stage dur' (\t -> f (s * t))

cycle : Stage ForATime a -> Stage Forever a
cycle (Stage dur f) =
  case dur of
    ForATime d -> Stage Forever (\t -> f (modFloat t d))
    _          -> Debug.crash "The impossible happened: Stage.cycle"

mkF d1 f1 f2 =
  \t -> if t <= d1 then f1 t else f2 (t - d1)

followedBy : Stage ForATime a -> Stage t a -> Stage t a
followedBy (Stage dur1 f1) (Stage dur2 f2) =
  case (dur1, dur2) of
    (ForATime d1, Forever)     -> Stage Forever (mkF d1 f1 f2)
    (ForATime d1, ForATime d2) -> Stage (ForATime (d1 + d2)) (mkF d1 f1 f2)
    (Forever, _)               -> Debug.crash "The impossible happened: Stage.followedBy"

chainTo : Stage ForATime a -> (a -> Stage t a) -> Stage t a
chainTo (Stage dur f) g =
  case dur of
    ForATime d -> Stage dur f `followedBy` g (f d)
    _          -> Debug.crash "The impossible happened: Stage.chainTo"

run : Signal (Stage Forever a) -> Signal Time -> Signal a
run s ts = Signal.map2 (\(t0, Stage _ f) t -> f (t - t0)) (Time.timestamp s) ts

map : (a -> b) -> Stage t a -> Stage t b
map g (Stage dur f) = Stage dur (g << f)

