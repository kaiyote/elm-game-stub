module Model exposing (..)


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


size : Size
size =
  Size 800 600


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


init : ( Model, Cmd msg )
init =
  model ! []
