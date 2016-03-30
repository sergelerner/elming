module Bingo where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (toUpper, repeat, trimRight)
import StartApp.Simple as StartApp


-- HELPERS

onInput : Signal.Address a -> (String -> a) -> Attribute
onInput address f =
    on "input" targetValue (\v -> Signal.message address (f v))


parseInt : String -> Int
parseInt string =
    case String.toInt string of
    Ok value ->
        value
    Err error ->
        0

-- MODEL

type alias Entry = 
    {
        phrase: String,
        points: Int,
        id: Int,
        wasSpoken: Bool
    }

type alias Model =
    {
        entries: List Entry,
        phraseInput: String, 
        pointInput: String,
        nextId: Int
    }

newEntry : String -> Int -> Int -> Entry
newEntry phrase points id =
    {
        phrase = phrase,
        points = points,
        id = id,
        wasSpoken = False
    }

initialModel: Model
initialModel = 
    {
        entries = [
            newEntry "Jhon" 33 1,
            newEntry "Jhoana" 22 2,
            newEntry "Jack" 28 3,
            newEntry "Jeff" 35 4
        ],
        phraseInput = "",
        pointInput = "",
        nextId = 5
    }    

-- UPDATE

type Action 
    = NoOp 
    | Sort 
    | Delete Int 
    | Mark Int
    | UpdatePhraseInput String
    | UpdatePointsInput String
    | Add

update : Action -> Model -> Model
update action model =
    
    case action of
        
        NoOp ->
            model
        
        Sort ->
            { model | entries = List.sortBy .points model.entries }  

        Delete id ->
            let 
                remainingEntries =
                    List.filter (\n -> n.id /= id) model.entries
            in
                { model | entries = remainingEntries }

        Mark id ->
            let
                updateEntry e =
                    if e.id == id then { e | wasSpoken = (not e.wasSpoken) } else e
            in 
                { model | entries = List.map updateEntry model.entries }

        UpdatePhraseInput contents ->
            { model | phraseInput = contents }

        UpdatePointsInput contents ->
            { model | pointInput = contents }    

        Add ->
            let
                entryToAdd = 
                    newEntry model.phraseInput (parseInt  model.pointInput) model.nextId
                
                isInvalid model =
                    String.isEmpty model.phraseInput || String.isEmpty model.pointInput
            in
                if isInvalid model
                then model
                else 
                    { model |
                        phraseInput = "",
                        pointInput = "",
                        entries = entryToAdd :: model.entries,
                        nextId = model.nextId + 1
                    }

-- VIEW
title : String -> Int -> Html
title message times =
    message ++ " "
        |> toUpper
        |> repeat times
        |> trimRight        
        |> text 
    -- Html.text (String.repeat times (String.toUpper message))

pageHeader : Html
pageHeader =
    h1 [ id "logo", class "classy" ] [ title "hi" 3 ]


totalPoints : List Entry -> Int
totalPoints entries = 
    let 
        spokenEntries = List.filter .wasSpoken entries
    in
        List.sum (List.map .points spokenEntries)

totalItem : Int -> Html
totalItem total = 
    li 
        [ class "total" ] 
        [
            span [ class "label"] [ text "Total:" ],
            span [ class "points"] [ text (toString total)  ]
        ]

entryItem : Signal.Address Action -> Entry -> Html
entryItem address entry = 
    li  [ 
            -- need to research classList
            classList [ ("highlight", entry.wasSpoken) ], 
            onClick address (Mark entry.id) 
        ]
        [  
            span [ class "phrase" ] [ text entry.phrase ],
            span [ class "points" ] [ text (toString entry.points) ],
            button [ class "delete", onClick address (Delete entry.id) ] [ text "x" ]
        ]

entryList : Signal.Address Action -> List Entry -> Html
entryList address entries =
    let
        entryItems = List.map (entryItem address) entries
        items = entryItems ++ [ totalItem (totalPoints entries) ]
    in
        ul [] items

entryForm : Signal.Address Action -> Model -> Html
entryForm address model = 
    div []
        [
            input  [ 
                        type' "text", 
                        placeholder "Phrase",
                        value model.phraseInput,
                        name "phrase",
                        autofocus True,
                        onInput address UpdatePhraseInput
                    ] [ ],
            input  [ 
                        type' "number", 
                        placeholder "Points",
                        value model.pointInput,
                        name "point",
                        onInput address UpdatePointsInput
                    ] [ ], 
            button [ class "add", onClick address Add ] [ text "Add"],
            h2 [] [ text (model.phraseInput ++ " " ++ model.pointInput) ]
        ]

view : Signal.Address Action -> Model -> Html
view address model =   
    div [ id "container", target "blank" ] 
        [ 
            pageHeader, 
            entryForm address model,
            entryList address model.entries,
            button [ class "sort", onClick address Sort ] [ text "sort" ]
        ]

-- RUN 

--main =
--    -- view ( update Sort initialModel )
--    initialModel
--        |> update Sort
--        |> view

main : Signal Html    
main =
    StartApp.start 
        {
            model  = initialModel,
            view   = view,
            update = update
        }    
    