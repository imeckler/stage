module Example where

import Stage
import Stage.Infix(..)
import Time(second)
import Time
import Graphics.Collage(..)
import Signal
import Color
import Debug

pos =  Stage.for (3 * second) (\t -> t * (40 / second))
    +> \x -> Stage.for (1 * second) (\t -> x - t * (x / (1 * second)))
    <> Stage.forever (\_ -> 0)

main = Stage.run (Signal.constant pos) (Time.every 30)
  |> Signal.map (\x -> filled Color.red (circle 20) |> moveX x)
  |> Signal.map (\form -> collage 300 300 [form])

