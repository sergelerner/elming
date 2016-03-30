import Graphics.Element exposing (..)
import Mouse
import Time


clickPosition : Signal (Int, Int)
clickPosition = 
    Signal.sampleOn Mouse.clicks Mouse.position


main : Signal Element
main =
    Signal.map show clickPosition