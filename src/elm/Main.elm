import Browser
import Complex as C
import Complex exposing (Complex)
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import List
import String as Str
import Url.Builder as Url

-- Main

main =
    Browser.sandbox { init = init, update = update, view = view}

-- Model

type alias Model =
    { a : Complex
    , b : Complex
    }

init : Model
init = { a = C.complex 1.89 0.09
       , b = C.complex 1.89 -0.09
       }

-- Update

type Msg = CenterAt Complex Complex

update : Msg -> Model -> Model
update (CenterAt a b) _ = { a = a, b = b }

-- View

view : Model -> Html Msg
view model =
    List.range -2 2
        |> List.map
              ( \offset ->
                    let
                        offsetA = toFloat offset |> (*) 0.01 |> C.imaginary |> C.add model.a
                    in
                        (renderedImage offsetA model.b)
              )
        |> div []

renderedImage : Complex -> Complex -> Html Msg
renderedImage a b =
    let
        params = [ a |> C.toCartesian >> .re >> Str.fromFloat >> Url.string "ax"
                 , a |> C.toCartesian >> .im >> Str.fromFloat >> Url.string "ay"
                 , b |> C.toCartesian >> .re >> Str.fromFloat >> Url.string "bx"
                 , a |> C.toCartesian >> .im >> Str.fromFloat >> Url.string "by"
                 , Url.int "size" 220
                 , Url.int "depth" 80
                 , Str.fromFloat 1.0 |> Url.string "epsilon"
                 ]
        sourceUrl = Url.crossOrigin "http://localhost:8420" ["render"] params
    in
        div []
            [ img [ src sourceUrl, onClick (CenterAt a b) ] []
            , div [] [text <| "a=" ++ C.toString a ++ ", b=" ++ C.toString b]
            ]
