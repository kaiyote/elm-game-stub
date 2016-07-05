module Main exposing (..)

import Html exposing (Html)
import Html.App as Html
import Svg exposing (Svg, svg, rect, text, text')
import Svg.Attributes exposing (height, width, x, y, fill)
import Keyboard exposing (KeyCode, downs, ups)
import AnimationFrame exposing (diffs)
import Time exposing (Time, every, second)
import Random exposing (Generator, bool, generate, int)
import List exposing (map, append, filter, length)
import Key exposing (..)


main : Program Never
main =
  Html.program
    { init = init
    , view = view size
    , update = update
    , subscriptions = subscriptions
    }


size : Size
size =
  Size 800 600


-- MODEL


type alias Size =
  { width : Int
  , height : Int
  }


type alias Model =
  { score : Int
  , dead : Bool
  , hero : Hero
  , enemies : List Enemy
  }


type alias Hero =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , isJumping : Bool
  , dir : Direction
  }


type alias Enemy =
  { x : Float
  , dir : Direction
  }


type Direction
  = Right
  | Left


model : Model
model =
  Model 0 False hero []


hero : Hero
hero =
  Hero 400 0 0 0 False Right


enemy : Direction -> Enemy
enemy dir =
  let
    x' =
      case dir of
        Right -> -20
        Left -> 820
  in
    Enemy x' dir


init : ( Model, Cmd Msg )
init =
  model ! []


-- UPDATE


type Msg
  = Update Time
  | Tick Time
  | EnemyMsg (Maybe Direction)
  | KeyDown KeyCode
  | KeyUp KeyCode
  | Restart


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Update dt ->
      let
        origCount =
          length model.enemies

        newHero =
          model.hero
            |> gravity dt
            |> jump
            |> walk
            |> physics model.dead dt

        newEnemiesList =
          model.enemies
            |> map (gPhysics dt)
            |> filter (collisionB model.hero)

        score =
          origCount - length newEnemiesList

        finalEnemiesList =
          filter onScreen newEnemiesList
      in
        { model
          | hero = newHero
          , enemies = finalEnemiesList
          , dead = model.dead || collisionA model
          , score = model.score + score } ! []

    Tick _ ->
      model ! [ generate EnemyMsg possibleDirection ]

    KeyDown keyCode ->
      case fromCode keyCode of
        Space ->
          update Restart model

        _ ->
          { model | hero = keyDown keyCode model.hero } ! []

    KeyUp keyCode ->
      { model | hero = keyUp keyCode model.hero } ! []

    EnemyMsg maybeDir ->
      case maybeDir of
        Nothing ->
          model ! []

        Just dir ->
          { model | enemies = (enemy dir) :: model.enemies } ! []

    Restart ->
      { model
        | hero = hero
        , enemies = []
        , score = 0
        , dead = False
      } ! []


keyDown : KeyCode -> Hero -> Hero
keyDown keyCode model =
  case fromCode keyCode of
    ArrowUp ->
      { model | isJumping = True }

    ArrowLeft ->
      { model | vx = -0.5 }

    ArrowRight ->
      { model | vx = 0.5 }

    _ ->
      model


keyUp : KeyCode -> Hero -> Hero
keyUp keyCode model =
  case fromCode keyCode of
    ArrowUp ->
      { model | isJumping = False }

    ArrowLeft ->
      { model | vx = 0 }

    ArrowRight ->
      { model | vx = 0 }

    _ ->
      model


gravity : Float -> Hero -> Hero
gravity dt hero =
  { hero | vy = if hero.y > 0 then hero.vy - (dt / jumpCoefficient) else 0 }


jump : Hero -> Hero
jump hero =
  { hero | vy = if hero.isJumping && hero.vy == 0 then 12.0 else hero.vy }


walk : Hero -> Hero
walk hero =
  case hero.vx of
    -0.5 -> { hero | dir = Left }
    0.5 -> { hero | dir = Right }
    _ -> hero


physics : Bool -> Float -> Hero -> Hero
physics dead dt hero =
  if dead then
    { hero
      | x = -100
      , y = -100
    }
  else
    { hero
      | x = clamp 0 780 (hero.x + (dt / walkCoefficient) * hero.vx)
      , y = max 0 (hero.y + (dt / jumpCoefficient) * hero.vy)
    }


collisionA : Model -> Bool
collisionA model =
  0 /= (model.enemies
          |> filter (killsHero model.hero)
          |> length)


killsHero : Hero -> Enemy -> Bool
killsHero hero enemy =
  enemy.x < hero.x + 20 && enemy.x + 20 > hero.x &&
    hero.y < 25 && hero.vy >= 0


collisionB : Hero -> Enemy -> Bool
collisionB hero enemy =
  not (hero.x < enemy.x + 20 && hero.x + 20 > enemy.x &&
    hero.y < 25 && hero.vy < 0)


gPhysics : Float -> Enemy -> Enemy
gPhysics dt enemy =
  let
    vx =
      case enemy.dir of
        Right -> 0.3
        Left -> -0.3
  in
    { enemy | x = enemy.x + (dt / walkCoefficient) * vx }


onScreen : Enemy -> Bool
onScreen enemy =
  case enemy.dir of
    Left ->
      enemy.x > -20

    Right ->
      enemy.x < 820


walkCoefficient : Float
walkCoefficient =
  4


jumpCoefficient : Float
jumpCoefficient =
  25


possibleDirection : Generator (Maybe Direction)
possibleDirection =
  Random.map2 (\a b -> if a then if b == 1 then Just Right else Just Left else Nothing) bool (int 0 1)


-- VIEW


view : Size -> Model -> Html Msg
view size model =
  let
    (w, h) =
      (toFloat size.width, toFloat size.height)

    (w', h') =
      (toString size.width, toString size.height)

    x' =
      toString (model.hero.x)

    y' =
      toString (h - model.hero.y - 50 - 30)
  in
    svg
      [ height h'
      , width w'
      ]
      ([ rect
        [ height h'
        , width w'
        , x "0"
        , y "0"
        , fill "#AEEEEE"
        ] []
      , rect
        [ height "50"
        , width w'
        , x "0"
        , y (toString (h - 50))
        , fill "#4AA72B"
        ] []
      , text'
        [ y "30"
        , x (toString (w - 50))
        , fill "#FFFFFF"
        ]
        [ text (toString model.score)
        ]
      , rect
        [ height "30"
        , width "20"
        , x x'
        , y y'
        , fill "#00FF00"
        ] []
      ] ++ (map (drawEnemy (w, h)) model.enemies))


drawEnemy : (Float, Float) -> Enemy -> Svg Msg
drawEnemy (w, h) enemy =
  let
    x' =
      toString enemy.x

    y' =
      toString (h - 50 - 25)
  in
    rect
      [ height "25"
      , width "20"
      , x x'
      , y y'
      , fill "#FF0000"
      ] []


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ diffs Update
    , every second Tick
    , downs KeyDown
    , ups KeyUp
    ]
