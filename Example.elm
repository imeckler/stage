module Example where

import Stage
import Stage.Infix(..)
import Time(second)
import Time
import Graphics.Collage(..)
import Signal
import Color
import Debug

pos =  Stage.cycle
    <| Stage.for (3 * second) (\t -> t * (40 / second))
    +> \x -> Stage.for (1 * second) (\t -> x - t * (x / (1 * second)))

main = Stage.run (Time.every 30) (Signal.constant pos)
  |> Signal.map (\x -> filled Color.red (circle 20) |> moveX x)
  |> Signal.map (\form -> collage 300 300 [form])

