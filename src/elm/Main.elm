import Browser
import Complex as C
import Complex exposing (Complex)
import Css exposing (displayFlex, flex, flexDirection)
import Html.Styled exposing (Html, Attribute, toUnstyled, div, img, text)
import Html.Styled.Attributes exposing (css, src)
import Html.Styled.Events exposing (onClick)
import List
import List.Extra as List
import Maybe
import Regex
import Round exposing (round)
import String as Str
import Url.Builder as Url

-- Main

main = Browser.sandbox { init = init, update = update, view = view >> toUnstyled}

-- Model

type alias Model =
    { centeredAt : FractalParameters
    }

type alias FractalParameters =
    { a : Complex
    , b : Complex
    }

init : Model
init = { centeredAt = { a = C.complex 1.89 0.09
                      , b = C.complex 1.89 -0.09
                      }
       }

-- Update

type Msg = ChangeCenter FractalParameters

update : Msg -> Model -> Model
update (ChangeCenter params) m = { m | centeredAt = params }

-- View

view : Model -> Html Msg
view {centeredAt} =
    div [ gridStyle ] (computeRows centeredAt)

computeRows : FractalParameters -> List (Html Msg)
computeRows fp =
    perturbHorizontal fp
        |> List.map (computeColumns >> div [ rowStyle ])

perturbHorizontal : FractalParameters -> List FractalParameters
perturbHorizontal ({a} as fp) =
    List.range -1 1
        |> List.map (\d -> { fp | a = toFloat d |> (*) 0.01 |> C.real |> C.add a })

computeColumns : FractalParameters -> List (Html Msg)
computeColumns fp =
    perturbVertical fp
        |> List.map fractalViewport

perturbVertical : FractalParameters -> List FractalParameters
perturbVertical ({a} as fp) =
    List.range -1 1
        |> List.map (\d -> { fp | a = toFloat d |> (*) 0.01 |> C.imaginary |> C.add a })

gridStyle : Attribute Msg
gridStyle =
    css [ displayFlex
        , flexDirection Css.row
        ]

rowStyle : Attribute Msg
rowStyle =
    css [ displayFlex
        , flex (Css.int 1)
        , flexDirection Css.column
        ]

-- Fractal viewports

complexRound : Int -> Complex -> String
complexRound n z =
    let
        censorZero s =
            let
                regex = Maybe.withDefault Regex.never (Regex.fromString "^[0.]+i?$")
            in
                if Regex.contains regex s then "" else s
        {re,im} = C.toCartesian z
        reStr = round n re |> censorZero
        imStr = round n im ++ "i" |> censorZero
        joinStr = if (reStr /= "") && (imStr /= "")
                  then " + " else ""
    in
        reStr ++ joinStr ++ imStr


fractalViewport : FractalParameters -> Html Msg
fractalViewport fp =
    let
        params = [ fp.a |> C.toCartesian >> .re >> Str.fromFloat >> Url.string "ax"
                 , fp.a |> C.toCartesian >> .im >> Str.fromFloat >> Url.string "ay"
                 , fp.b |> C.toCartesian >> .re >> Str.fromFloat >> Url.string "bx"
                 , fp.a |> C.toCartesian >> .im >> Str.fromFloat >> Url.string "by"
                 , Url.int "size" 220
                 , Url.int "depth" 80
                 , Str.fromFloat 1.0 |> Url.string "epsilon"
                 ]
        sourceUrl = Url.crossOrigin "http://localhost:8420" ["render"] params
    in
        div []
            [ img [ src sourceUrl, onClick (ChangeCenter fp) ] []
            , div [] [text <| "a=" ++ complexRound 2 fp.a ++ ", b=" ++ complexRound 2 fp.b]
            ]
