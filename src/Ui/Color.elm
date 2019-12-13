module Ui.Color exposing
    ( primary
    , secondary1
    , secondary2
    , complement
    , white
    , linkBlue
    )

{-| The color palette of the app. Colors are based off of this palette <https://paletton.com/#uid=6550T0kbnGvapVrbnOV9Xwsc4o0>.

@docs primary
@docs secondary1
@docs secondary2
@docs complement
@docs white
@docs linkBlue

-}

import Element exposing (Color)


{-| A pink hue
-}
primary : Color
primary =
    Element.rgb255 245 165 219


{-| An orange hue
-}
secondary1 : Color
secondary1 =
    Element.rgb255 255 202 172


{-| A purple hue
-}
secondary2 : Color
secondary2 =
    Element.rgb255 187 172 241


{-| A green hue
-}
complement : Color
complement =
    Element.rgb255 230 253 170


{-| Solid white
-}
white : Color
white =
    Element.rgb 1 1 1


{-| The same blue as a standard HTML link
-}
linkBlue : Color
linkBlue =
    Element.rgb 0 0 0.8
