module Utilities (restrictLevel, safeStrToLevel) where

import String

safeStrToLevel : String -> Int
safeStrToLevel =
  String.toInt >> Result.toMaybe >> Maybe.withDefault 1

restrictLevel : Int -> Int
restrictLevel level =
  if level < 1 then
    1 
  else if level > 20 then
    20
  else
    level
