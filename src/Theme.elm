module Theme exposing (Layout, Palette, Styles, Theme, createTheme, headline, overrideCss)

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
    , accent : Color
    , warning : Color
    , appBackground : Color
    , lightBackground : Color
    , mediumBackground : Color
    }


createColorPalette : Palette
createColorPalette =
    { major = hex "29BF89"
    , lightText = hex "FFFFFF"
    , darkText = hex "000000"
    , dark = hex "7E8D85"
    , accent = hex "0083BB"
    , warning = hex "B60606"
    , appBackground = rgba 255 255 255 0.9
    , lightBackground = hex "FFFFFF"
    , mediumBackground = hex "E3E3E3"
    }


createTheme : Theme
createTheme =
    let
        layout =
            { defaultMargin = rem 1.25
            , smallMargin = rem 0.5
            , compactMargin = rem 0.75
            , sidebarWidth = rem 15
            , secondSidebarWidth = rem 9
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


headline : Theme -> Style
headline theme =
    Css.batch [ theme.styles.headlineFontFamily, theme.styles.headlineFontWeight ]


overrideCss : List Style -> List Style -> Attribute msg
overrideCss styles overrides =
    css (List.concat [ styles, overrides ])
