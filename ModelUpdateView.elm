import Graphics.Element exposing (..)
import Keyboard
import Mouse

-- MODEL

type alias Model = Int

initialModel : Model
initialModel = 0


-- UPDATE

update : a -> Model -> Model
update event model =
    model + 1


model: Signal Model
model = 
    Signal.foldp update initialModel Keyboard.presses


-- VIEW

view : Model -> Element
view model = 
    show model

 
main : Signal Element
main =
    Signal.map view model 