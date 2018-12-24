import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Platform.Cmd
import Time
import Collage exposing (group, shift, circle, rectangle, filled, uniform)
import Collage.Layout exposing (at, topLeft)
import Collage.Render exposing (svg)
import Color
import Dict
import Maybe exposing (withDefault)

main : Program () Model Msg
main =
    Browser.element {
      init = init,
      update = update,
      view = view,
      subscriptions = subscriptions
    }

type Msg = Tick
type alias Model = { particles: List Particle, springs: List Spring }

type alias Point = (Float, Float)
type Particle = Particle Point Point

sub : Point -> Point -> Point
sub (ax, ay) (bx, by) = (ax - bx, ay - by)

add : Point -> Point -> Point
add (ax, ay) (bx, by) = (ax + bx, ay + by)

mul_scalar : Point -> Float -> Point
mul_scalar (ax, ay) c = (c * ax, c * ay)

type alias Spring = { i1 : Int,  i2 : Int }

init : () -> (Model, Cmd Msg)
init _ =
  ( {
    particles = [Particle (0,0) (0,0), Particle (-200,-200) (0,0), Particle (-200,0) (0,0)],
    springs = [{i1 = 0, i2 = 1}, {i1 = 1, i2 = 2}]
  }, Platform.Cmd.none
  )

nth : List a -> Int -> a -> a
nth xs n default = case List.head (List.drop n xs) of
  Just x -> x
  Nothing -> default

nthParticle : List Particle -> Int -> Particle
nthParticle particles n = nth particles n (Particle (0, 0) (0, 0))

computeSpringForce : Spring -> List Particle -> Point
computeSpringForce spring particles =
  let
    i1 = spring.i1
    i2 = spring.i2
    (Particle aPos aVel) = nthParticle particles i1
    (Particle bPos bVel) = nthParticle particles i2
    dir = sub bPos aPos
    k = 0.1
    in
    mul_scalar dir k

computeParticleForces : List Spring -> List Particle -> Dict.Dict Int Point
computeParticleForces springs particles =
  let
    doUpdate : Spring -> Dict.Dict Int Point -> Dict.Dict Int Point
    doUpdate spring forces =
      let
        f = computeSpringForce spring particles
        combineForces newForce existing = case existing of
          (Just oldForce) -> Just (add newForce oldForce)
          Nothing -> Just newForce
        forces1 = Dict.update spring.i1 (combineForces f) forces
        forces2 = Dict.update spring.i2 (combineForces (mul_scalar f -1)) forces1
      in
        forces2
  in
    List.foldl doUpdate Dict.empty springs

--
-- computeForce : Model -> List Point


update : Msg -> Model -> (Model, Platform.Cmd.Cmd Msg)
update _ model =
  let
    forces = computeParticleForces model.springs model.particles
  in
    ( { particles = List.indexedMap (\i p -> stepParticle p (withDefault (0,0) (Dict.get i forces))) model.particles
      , springs = model.springs
      }
    , Platform.Cmd.none
    )


view : Model -> Html Msg
view model =
    let
        particles = List.map viewParticle model.particles

        springs = List.map (viewSpring model.particles) model.springs

        rect =
            rectangle 600 400
                |> filled (uniform Color.grey)
    in
        -- svg (shift (10.0 + model * 10, 10.0) circ)
        svg (group (particles ++ springs ++ [rect]))


stepParticle (Particle p v) f =
  let
    h = 0.033
    v1 = add v (mul_scalar f h)
    p1 = add p (mul_scalar v1 h)
  in
    Particle p1 v1

viewParticle (Particle loc _) = shift loc (circle 20 |> filled (uniform Color.red))

viewSpring particles spring =
  let
    (Particle apos _) = nthParticle particles spring.i1
    (Particle bpos _) = nthParticle particles spring.i2
    in
      Collage.path [ apos, bpos ]
        |> Collage.traced (Collage.solid Collage.thin (uniform Color.blue))

subscriptions model =
  Time.every 10 (\_ -> Tick)
