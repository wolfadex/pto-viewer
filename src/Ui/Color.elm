module Ui.Color exposing (complement, primary, secondary1, secondary2, white)

import Element exposing (Color)


primary : Color
primary =
    Element.rgb255 245 165 219


secondary1 : Color
secondary1 =
    Element.rgb255 255 202 172


secondary2 : Color
secondary2 =
    Element.rgb255 187 172 241


complement : Color
complement =
    Element.rgb255 230 253 170


white : Color
white =
    Element.rgb 1 1 1
