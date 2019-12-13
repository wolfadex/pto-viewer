module Ui exposing
    ( button
    , numberInt
    , text
    , modal
    )

{-| All the generic UI elements, such as buttons.


## Inputs

@docs button
@docs numberInt
@docs text


## Layout

@docs modal

-}

import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input exposing (Label)
import Html.Attributes
import Ui.Color as Color



---- INPUTS ----


{-| A basic button with a base background color of `Ui.Color.secondary1`.
-}
button : List (Attribute msg) -> { onPress : Maybe msg, label : Element msg } -> Element msg
button attributes =
    Input.button
        ([ Border.shadow
            { blur = 10
            , color = Element.rgba 0 0 0 0.05
            , offset = ( 0, 2 )
            , size = 1
            }
         , Background.color Color.secondary1
         , Border.rounded 4
         , Element.paddingXY 16 8
         ]
            ++ attributes
        )


{-| A number input that only accepts `Int`s
-}
numberInt : List (Attribute msg) -> { onChange : Int -> msg, value : Int, label : Label msg } -> Element msg
numberInt attributes { onChange, value, label } =
    Input.text
        ([] ++ attributes ++ [ Element.htmlAttribute <| Html.Attributes.type_ "number" ])
        { onChange =
            \maybeInt ->
                case String.toInt maybeInt of
                    Just n ->
                        onChange n

                    Nothing ->
                        onChange value
        , text = String.fromInt value
        , placeholder = Nothing
        , label = label
        }


{-| Simple text input
-}
text : List (Attribute msg) -> { onChange : String -> msg, value : String, label : Label msg } -> Element msg
text attributes { onChange, value, label } =
    Input.text
        ([] ++ attributes)
        { onChange = onChange
        , text = value
        , placeholder = Nothing
        , label = label
        }



---- LAYOUT ----


{-| A basic modal. Should be paird with `Element.inFront`

    Element.layout
        [ Element.inFront <|
            Ui.modal <|
                Element.text "Hello Modal"
        ]
        (...)

-}
modal : Element msg -> Element msg
modal child =
    Element.el
        [ Background.color <| Element.rgba 0 0 0 0.5
        , Element.width Element.fill
        , Element.height Element.fill
        ]
        (Element.el
            [ Border.shadow
                { blur = 10
                , color = Element.rgba 0 0 0 0.05
                , offset = ( 0, 2 )
                , size = 1
                }
            , Border.rounded 4
            , Element.paddingXY 16 8
            , Background.color Color.white
            , Element.centerX
            , Element.centerY
            , Element.clip
            , Element.width Element.shrink
            ]
            child
        )
