import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Platform.Cmd
import Time
import Collage exposing (group, shift, circle, rectangle, filled, uniform)
import Collage.Layout exposing (at, topLeft)
import Collage.Render exposing (svg)
import Color

main : Program () Model Msg
main =
    Browser.element {
      init = init,
      update = update,
      view = view,
      subscriptions = subscriptions
    }

type Msg = Tick
type alias Model = List Particle

type alias Point = (Float, Float)
type Particle = Particle Point Point

init : () -> (Model, Cmd Msg)
init _ =
  ( [Particle (0,0) (0,0), Particle (-200,-200) (10,10)]
  , Platform.Cmd.none
  )


update : Msg -> Model -> (Model, Platform.Cmd.Cmd Msg)
update _ particles = (List.map stepParticle particles, Platform.Cmd.none)


view : Model -> Html Msg
view particles =
    let
        circ =
            circle 50
                |> filled (uniform Color.red)

        rect =
            rectangle 600 400
                |> filled (uniform Color.grey)
    in
        -- svg (shift (10.0 + model * 10, 10.0) circ)
        svg (group ((List.map viewParticle particles) ++ [rect]))


stepParticle (Particle (x, y) (vx, vy) as p) =
  let h = 0.033 in
  let f = computeForce p particles
    Particle (x + h * vx, y + h * vy) (vx, vy)
viewParticle (Particle loc _) = shift loc (circle 20 |> filled (uniform Color.red))



subscriptions model =
  Time.every 10 (\_ -> Tick)
