module Theme exposing (Layout, Palette, Styles, Theme, createTheme, overrideCss)

import Css exposing (..)
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes exposing (css)


type alias Styles =
    { navigationHeight : Style
    , contentPadding : Style
    , contentSidePadding : Style
    , blockMargin : Style
    , headlineFontFamily : Style
    , headlineFontWeight : Style
    , contentFontFamily : Style
    }


type alias Theme =
    { colors : Palette
    , layout : Layout
    , styles : Styles
    }


type alias Layout =
    { defaultMargin : Rem
    , smallMargin : Rem
    , compactMargin : Rem
    , sidebarWidth : Rem
    , secondSidebarWidth : Rem
    , navigationHeight : Rem
    }


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


createTheme : Theme
createTheme =
    let
        layout =
            { defaultMargin = rem 1.25
            , smallMargin = rem 0.75
            , compactMargin = rem 0.75
            , sidebarWidth = rem 15
            , secondSidebarWidth = rem 12
            , navigationHeight = rem 3.25
            }

        styles =
            { navigationHeight = height layout.navigationHeight
            , contentPadding = padding2 layout.smallMargin layout.compactMargin
            , contentSidePadding = padding2 (px 0) layout.compactMargin
            , blockMargin = margin2 layout.smallMargin layout.compactMargin
            , headlineFontFamily = fontFamilies [ "Work Sans", "serif" ]
            , headlineFontWeight = fontWeight (int 900)
            , contentFontFamily = fontFamilies [ "Work Sans", "serif" ]
            }
    in
    { colors = createColorPalette
    , layout = layout
    , styles = styles
    }


overrideCss : List Style -> List Style -> Attribute msg
overrideCss styles overrides =
    css (List.concat [ styles, overrides ])
