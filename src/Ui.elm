module Ui exposing (button, numberInt, modal, text)

import Element exposing (Attribute, Element)
import Element.Background as Background
import Html.Attributes
import Element.Border as Border
import Element.Input as Input exposing (Label)
import Ui.Color as Color
import Json.Decode


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


numberInt : List (Attribute msg) -> { onChange : Int -> msg, value : Int, label : Label msg } -> Element msg
numberInt attributes { onChange, value, label } =
    Input.text
        ([] ++ attributes ++ [ Element.htmlAttribute <| Html.Attributes.type_ "number"])
        { onChange =
            (\maybeInt ->
                case String.toInt maybeInt of
                    Just n -> onChange n
                    Nothing -> onChange value
            )
        , text = String.fromInt value
        , placeholder = Nothing
        , label = label
        }

text : List (Attribute msg) -> { onChange : String -> msg, value : String, label : Label msg } -> Element msg
text attributes { onChange, value, label } =
    Input.text
        ([] ++ attributes)
        { onChange = onChange
        , text = value
        , placeholder = Nothing
        , label = label
        }

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
            ]
            child
        )
