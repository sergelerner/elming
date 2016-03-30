import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StartApp.Simple as StartApp
import Debug

model = 0


type Action = Increase | Decrease


update action model = 
    case Debug.watch "Action" action of
        Increase -> 
            model + 1
        Decrease -> 
            model - 1


view address model =
    div []
        [
            button [ onClick address Increase] [ text "+" ],
            span [] [ text (toString model) ],
            button [ onClick address Decrease] [ text "-" ]

        ]


main = 
    StartApp.start { model = model, view = view, update = update }
