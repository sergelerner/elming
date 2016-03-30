import Graphics.Element exposing (..)
import Mouse
import Window
import Keyboard
import Time
import Char


area : (Int, Int) -> Int
area (x,y) = 
    x * y


windowArea : Signal Int
windowArea = 
    Signal.map area Window.dimensions


charachters : Signal Char
charachters = 
    Signal.map Char.fromCode Keyboard.presses


pressedDigits : Signal Bool
pressedDigits = 
    Signal.map Char.isDigit charachters    


main : Signal Element
main =
    --Signal.map show windowArea
    Signal.map show pressedDigits  