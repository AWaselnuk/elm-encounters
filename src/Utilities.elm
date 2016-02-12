module Utilities (restrictLevel, restrictRating, restrictXP, safeStrToLevel, safeRatingToXP) where

import String
import Dict

safeStrToLevel : String -> Int
safeStrToLevel =
  String.toInt >> Result.toMaybe >> Maybe.withDefault 0

restrictLevel : Int -> Int
restrictLevel level =
  clamp 1 20 level

restrictXP : Int -> Int
restrictXP xp =
  clamp 0 highestXP xp

-- TODO: Implement this. Should i just use strings with a select box?
restrictRating : Float -> Float
restrictRating rating =
  rating

safeRatingToXP : Float -> Int
safeRatingToXP rating =
  Dict.get rating ratingXPTable |> Maybe.withDefault 0

highestXP : Int
highestXP =
  List.maximum xpList |> Maybe.withDefault 0

ratingXPTable : Dict.Dict Float Int
ratingXPTable =
  Dict.fromList ratingXpList

ratingList : List Float
ratingList = 
  List.map fst ratingXpList

xpList : List Int
xpList = 
  List.map snd ratingXpList

ratingXpList : List (Float, Int)
ratingXpList = 
  [ (0, 10)
  , (0.125, 25)
  , (0.25, 50)
  , (0.5, 100)
  , (1, 200)
  , (2, 450)
  , (3, 700)
  , (4, 1100)
  , (5, 1800)
  , (6, 2300)
  , (7, 2900)
  , (8, 3900)
  , (9, 5000)
  , (10, 5900)
  , (11, 7200)
  , (12, 8400)
  , (13, 10000)
  , (14, 11500)
  , (15, 13000)
  , (16, 15000)
  , (17, 18000)
  , (18, 20000)
  , (19, 22000)
  , (20, 25000)
  , (21, 33000)
  , (22, 41000)
  , (23, 50000)
  , (24, 62000)
  , (25, 75000)
  , (26, 90000)
  , (27, 105000)
  , (28, 120000)
  , (29, 135000)
  , (30, 155000)
  ]

