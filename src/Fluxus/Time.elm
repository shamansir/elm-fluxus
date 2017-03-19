module Fluxus.Time exposing (..)

type alias TimePosition =
    { total: Float
    , delta: Float
    }

init : TimePosition
init =
    { total = 0
    , delta = 0
    }

advance : Float -> TimePosition -> TimePosition
advance dt timePos =
    { timePos
    | delta = dt
    , total = timePos.total + dt
    }

inSeconds : TimePosition -> Float
inSeconds { total } = total / 1000

deltaInSeconds : TimePosition -> Float
deltaInSeconds { delta } = delta / 1000
