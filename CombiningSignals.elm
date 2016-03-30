import Graphics.Element exposing (..)
import Mouse
import Window


view : Int -> Int -> Element
view x width =
    let
        side = 
            if x < width // 2 then "LEFT" else "RIGHT"
    in
        show side
        

main : Signal Element
main = 
    Signal.map2 view Mouse.x Window.width