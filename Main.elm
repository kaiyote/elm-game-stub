module Main exposing (main)

import Html.App as Html
import EnemySmash.Model as ESModel
import EnemySmash.View as ESView
import EnemySmash.Update as ESUpdate
import Rogueish.Model as RModel
import Rogueish.View as RView
import Rogueish.Update as RUpdate
import Html exposing (..)
import Html.Events exposing (..)


type ActiveGame
  = None
  | EnemySmash
  | Rogueish


type alias Model =
  { active : ActiveGame
  , smashModel : ESModel.Model
  , rogueModel : RModel.Model
  }


main : Program Never
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


init : ( Model, Cmd msg )
init =
  Model None ESModel.model RModel.model ! []


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  case .active model of
    None ->
      Sub.none

    EnemySmash ->
      ESUpdate.subscriptions model.smashModel
        |> Sub.map ESMsg

    Rogueish ->
      Sub.none


-- UPDATE


type Msg
  = Activate ActiveGame
  | ESMsg ESUpdate.Msg
  | RMsg RUpdate.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Activate game ->
      { model
      | active = game
      , smashModel = ESModel.model
      } ! []

    ESMsg msg ->
      let
        (newModel, cmds) =
          ESUpdate.update msg model.smashModel
      in
        { model | smashModel = newModel } ! [ Cmd.map ESMsg cmds ]

    RMsg msg ->
      let
        (newModel, cmds) =
          RUpdate.update msg model.rogueModel
      in
        { model | rogueModel = newModel } ! [ Cmd.map RMsg cmds ]



-- VIEW


view : Model -> Html Msg
view model =
  let
    activeView =
      case .active model of
        None ->
          div [] []

        EnemySmash ->
          ESView.view model.smashModel
            |> Html.map ESMsg

        Rogueish ->
          RView.view model.rogueModel
            |> Html.map RMsg
  in
    div []
      [ button [ onClick (Activate EnemySmash) ] [ text "Enemy Smash" ]
      , button [ onClick (Activate Rogueish) ] [ text "Rougeish" ]
      , button [ onClick (Activate None) ] [ text "Off" ]
      , div [] [ activeView ]
      ]
