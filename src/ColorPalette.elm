module ColorPalette exposing (Palette, createColorPalette)

import Css exposing (..)


type alias Palette =
    { major : Color
    , lightText : Color
    , darkText : Color
    , dark : Color
    , secondary : Color
    , accent : Color
    , accentLight : Color
    , appBackground : Color
    , lightBackground : Color
    , mediumBackground : Color
    }


createColorPalette : Palette
createColorPalette =
    { major = hex "99B898"
    , lightText = hex "FFFFFF"
    , darkText = hex "000000"
    , dark = hex "2A363B"
    , secondary = hex "FECE8A"
    , accent = hex "E84A5F"
    , accentLight = hex "FF847C"
    , appBackground = rgba 255 255 255 0.8
    , lightBackground = hex "FFFFFF"
    , mediumBackground = hex "F9F9F9"
    }
