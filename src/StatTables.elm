module StatTables exposing (xpRatingTable, ratingXPTable, ratingList, xpList, levelList, easyThresholds, mediumThresholds, hardThresholds, deadlyThresholds)

import String
import Dict exposing (Dict)

-- Levels

levelList : List Int
levelList = [1..20]

-- XP and Rating Tables

xpRatingTable : Dict.Dict Int Float
xpRatingTable =
  Dict.fromList <| List.map (\row -> (snd row, fst row)) ratingXpList

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

-- Thresholds

easyThresholds : Dict Int Int
easyThresholds =
  Dict.fromList
    [
      (1, 25)
    , (2, 50)
    , (3, 75)
    , (4, 125)
    , (5, 250)
    , (6, 300)
    , (7, 350)
    , (8, 450)
    , (9, 550)
    , (10, 600)
    , (11, 800)
    , (12, 1000)
    , (13, 1100)
    , (14, 1250)
    , (15, 1400)
    , (16, 1600)
    , (17, 2000)
    , (18, 2100)
    , (19, 2400)
    , (20, 2800)
    ]

mediumThresholds : Dict Int Int
mediumThresholds =
  Dict.fromList
    [
      (1, 50)
    , (2, 100)
    , (3, 150)
    , (4, 250)
    , (5, 500)
    , (6, 600)
    , (7, 750)
    , (8, 900)
    , (9, 1100)
    , (10, 1200)
    , (11, 1600)
    , (12, 2000)
    , (13, 2200)
    , (14, 2500)
    , (15, 2800)
    , (16, 3200)
    , (17, 3900)
    , (18, 4200)
    , (19, 4900)
    , (20, 5700)
    ]

hardThresholds : Dict Int Int
hardThresholds =
  Dict.fromList
    [
      (1, 75)
    , (2, 150)
    , (3, 225)
    , (4, 375)
    , (5, 750)
    , (6, 900)
    , (7, 1100)
    , (8, 1400)
    , (9, 1600)
    , (10, 1900)
    , (11, 2400)
    , (12, 3000)
    , (13, 3400)
    , (14, 3800)
    , (15, 4300)
    , (16, 4800)
    , (17, 5900)
    , (18, 6300)
    , (19, 7300)
    , (20, 8500)
    ]

deadlyThresholds : Dict Int Int
deadlyThresholds =
  Dict.fromList
    [
      (1, 100)
    , (2, 200)
    , (3, 400)
    , (4, 500)
    , (5, 1100)
    , (6, 1400)
    , (7, 1700)
    , (8, 2100)
    , (9, 2400)
    , (10, 2800)
    , (11, 3600)
    , (12, 4500)
    , (13, 5100)
    , (14, 5700)
    , (15, 6400)
    , (16, 7200)
    , (17, 8800)
    , (18, 9500)
    , (19, 10900)
    , (20, 12700)
    ]

