module EnemySmash.Update exposing (update, subscriptions)

import EnemySmash.Model exposing (Direction(Right, Left), Msg(..), Model, Enemy, Hero, hero, enemy)
import Key exposing (..)
import List exposing (length, map, filter)
import Random exposing (Generator, bool, generate, int)
import Keyboard exposing (KeyCode)
import Time exposing (Time)
import AnimationFrame as AF


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ AF.diffs Update
    , Time.every Time.second Tick
    , Keyboard.downs KeyDown
    , Keyboard.ups KeyUp
    ]


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
            |> filter (enemiesHeroMissed model.hero)

        score =
          origCount - length newEnemiesList

        finalEnemiesList =
          filter onScreen newEnemiesList
      in
        { model
          | hero = newHero
          , enemies = finalEnemiesList
          , dead = model.dead || enemiesTouchingHero model
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


enemiesTouchingHero : Model -> Bool
enemiesTouchingHero model =
  0 /= (model.enemies
          |> filter (killsHero model.hero)
          |> length)


killsHero : Hero -> Enemy -> Bool
killsHero hero enemy =
  enemy.x < hero.x + 20 && enemy.x + 20 > hero.x &&
    hero.y < 25 && hero.vy >= 0


enemiesHeroMissed : Hero -> Enemy -> Bool
enemiesHeroMissed hero enemy =
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
