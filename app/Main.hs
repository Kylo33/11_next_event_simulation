{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Data.List (sortOn)
import System.Random (mkStdGen, randoms)

data EventType = ReviewEvent | DemandEvent | DeliveryEvent Int

data SimState = SimState
  { time :: Double,
    inventory :: Int,
    onOrder :: Int,
    backOrderCount :: Int,
    orderCount :: Int
  }

exponential :: Double -> Double -> Double
exponential mu u = (-mu) * log (1 - u)

geometric :: Double -> Double -> Int
geometric p u = floor $ log (1 - u) / log (p)

inventoryMax :: Int
inventoryMax = 80

inventoryMin :: Int
inventoryMin = 20

main :: IO ()
main = do
  putStrLn $ "Reports: "
  putStrLn $ "t: " ++ (show . time $ endState)
  putStrLn $ "orderCount: " ++ (show . orderCount $ endState)
  putStrLn $ "backOrderCount : " ++ (show . backOrderCount $ endState)
  where
    r : rs = randoms (mkStdGen 8198) :: [Double]
    beginState =
      SimState
        { time = 0,
          inventory = 0,
          onOrder = 0,
          backOrderCount = 0,
          orderCount = 0
        }
    initialEvents = [(ReviewEvent, 7), (DemandEvent, exponential (2 / 3) r)]
    endState = snd . uncurry refill $ simulate rs initialEvents beginState

simulate :: [Double] -> [(EventType, Double)] -> SimState -> ([Double], SimState)
simulate rand [] s = (rand, s)
simulate rand _ s | time s >= 1000 = (rand, s)
simulate rand (e : es) s = simulate rand' (sortOn snd (es ++ es')) s'
  where
    (rand', es', s') = processEvent rand (fst e) (s {time = snd e})

processEvent :: [Double] -> EventType -> SimState -> ([Double], [(EventType, Double)], SimState)
processEvent rand@(u : us) ReviewEvent s
  | inventory s + onOrder s > inventoryMin = (rand, [newReviewEvent], s)
  | otherwise =
      let orderAmt = inventoryMax - onOrder s - inventory s
          newDeliveryEvent = (DeliveryEvent (orderAmt), exponential 7 u)
          newState = s {onOrder = onOrder s + orderAmt, orderCount = orderCount s + 1}
       in (us, [newReviewEvent, newDeliveryEvent], newState)
  where
    newReviewEvent = (ReviewEvent, time s + 7)
processEvent rand (DeliveryEvent count) s = (rand, [], newState)
  where
    newState = s {onOrder = onOrder s - count, inventory = inventory s + count}
processEvent (u1 : u2 : us) DemandEvent s
  | purchased <= 0 = (us, [newDemandEvent], s)
  | otherwise =
      let backOrdered = max 0 $ purchased - inventory s
          newState =
            s
              { inventory = inventory s - purchased,
                backOrderCount = backOrderCount s + backOrdered
              }
       in (us, [newDemandEvent], newState)
  where
    purchased = geometric (3 / 4) u1
    newDemandEvent = (DemandEvent, time s + exponential (2 / 3) u2)

refill :: [Double] -> SimState -> ([Double], SimState)
refill rand@(r : rs) s@SimState {inventory = i, orderCount = o, backOrderCount = b, time = t}
  | i < inventoryMax = (rs, s {orderCount = o + 1, backOrderCount = b - min 0 i, time = t + exponential 7 r})
  | otherwise = (rand, s)
