module Gopm.Util exposing (formatDate, formatName)

import Time exposing (toDay, toMonth, toYear, utc, Month(..))
import String.Extra exposing (replace)

formatDate : Time.Posix -> String
formatDate time =
    String.fromInt (toDay utc time)
        ++ ":"
        ++ toFrenchMonth (toMonth utc time)
        ++ ":"
        ++ String.fromInt (toYear utc time)

toFrenchMonth : Month -> String
toFrenchMonth month =
  case month of
    Jan -> "janvier"
    Feb -> "février"
    Mar -> "mars"
    Apr -> "avril"
    May -> "mai"
    Jun -> "juin"
    Jul -> "juillet"
    Aug -> "août"
    Sep -> "septembre"
    Oct -> "octobre"
    Nov -> "novembre"
    Dec -> "décembre"

formatName : String -> String
formatName =
    replace "_" " "
