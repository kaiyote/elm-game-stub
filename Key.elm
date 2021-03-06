module Key exposing (..)


type Key
  = Space
  | ArrowLeft
  | ArrowUp
  | ArrowRight
  | Unknown


fromCode : Int -> Key
fromCode keyCode =
  case keyCode of
    32 ->
      Space

    37 ->
      ArrowLeft

    38 ->
      ArrowUp

    39 ->
      ArrowRight

    _ ->
      Unknown
