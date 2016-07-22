module Rogueish.MapGen3 exposing (..)

(\\) : number -> number -> number
(\\) left right =
  (left + right - 1) // right
