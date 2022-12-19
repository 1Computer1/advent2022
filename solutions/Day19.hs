module Main (main) where

import Advent (input)
import Control.Monad (guard)
import Data.Char (isNumber)
import Data.List (foldl')
import GHC.Generics (Generic)
import Optics

main :: IO ()
main = do
    i <- $(input)
    print $ p1 i
    print $ p2 i

data Resource = Ore | Cla | Obs | Geo

data Bp = Bp
    { ore_ore :: !Int
    , ore_cla :: !Int
    , ore_obs, cla_obs :: !Int
    , ore_geo, obs_geo :: !Int
    }

costOf :: Resource -> Bp -> (Int, Int, Int)
costOf Ore bp = (bp.ore_ore, 0,          0         )
costOf Cla bp = (bp.ore_cla, 0,          0         )
costOf Obs bp = (bp.ore_obs, bp.cla_obs, 0         )
costOf Geo bp = (bp.ore_geo, 0,          bp.obs_geo)

data Inv = Inv { bots, num :: !Int }
    deriving (Generic)

emptyInv :: Inv
emptyInv = Inv { bots = 0, num = 0 }

data Step = Step
    { time :: !Int
    , ores, clas, obss, geos :: !Inv
    }
    deriving (Generic)

initStep :: Step
initStep = Step
    { time = 0
    , ores = emptyInv { bots = 1 }
    , clas = emptyInv
    , obss = emptyInv
    , geos = emptyInv
    }

updateStep :: Step -> Step
updateStep st = st
    & #time %~ succ
    & #ores % #num %~ (+ (st ^. #ores % #bots))
    & #clas % #num %~ (+ (st ^. #clas % #bots))
    & #obss % #num %~ (+ (st ^. #obss % #bots))
    & #geos % #num %~ (+ (st ^. #geos % #bots))

spendResources :: (Int, Int, Int) -> Step -> Step
spendResources (x, y, z) st = st
    & #ores % #num %~ (- x)
    & #clas % #num %~ (- y)
    & #obss % #num %~ (- z)

addBot :: Resource -> Step -> Step
addBot b st = case b of
    Ore -> st & #ores % #bots %~ succ
    Cla -> st & #clas % #bots %~ succ
    Obs -> st & #obss % #bots %~ succ
    Geo -> st & #geos % #bots %~ succ

parse :: String -> [Bp]
parse = map (f . map read . filter (all isNumber) . words) . lines
    where
        f [ore, cla, obs1, obs2, geo1, geo2] = Bp ore cla obs1 obs2 geo1 geo2

simulate :: Int -> Bp -> Int
simulate maxT bp = go 0 initStep (False, False, False)
    where
        (maxOre, maxCla, maxObs) = (maximum [bp.ore_ore, bp.ore_cla, bp.ore_obs, bp.ore_geo], bp.cla_obs, bp.obs_geo)

        nextStates best step (!couldOre, !couldCla, !couldObs) = do
            let timeLeft = maxT - step.time
            guard $ timeLeft > 0
            -- heuristic: keep only states that have a chance at getting more geodes than current best
            -- i.e. with their current geos, they add as many geos as possible from existing geo bots,
            -- and they somehow get a geo bot every remaining minute, so 1 + ... + timeLeft more geos
            guard $ step.geos.num + step.geos.bots * timeLeft + (timeLeft * (timeLeft - 1) `div` 2) > best
            -- always a build a geo bot when we can, it is less optimal otherwise
            if step.ores.num >= bp.ore_geo && step.obss.num >= bp.obs_geo then do
                let step' = addBot Geo . updateStep . spendResources (costOf Geo bp) $ step
                pure (step', (False, False, False))
            else do
                let canOre = step.ores.num >= bp.ore_ore
                let canCla = step.ores.num >= bp.ore_cla
                let canObs = step.ores.num >= bp.ore_obs && step.clas.num >= bp.cla_obs
                shouldBuild <- [True, False]
                if shouldBuild then do
                    -- if this state wants to build a bot but the previous state could have built the same bot,
                    -- then this state is strictly less optimal than building the bot at the previous state
                    -- also, dont build more than maximum rate of use
                    res <- [Ore | not couldOre, canOre, step.ores.bots < maxOre]
                        <> [Cla | not couldCla, canCla, step.clas.bots < maxCla]
                        <> [Obs | not couldObs, canObs, step.obss.bots < maxObs]
                    let step' = addBot res . updateStep . spendResources (costOf res bp) $ step
                    pure (step', (False, False, False))
                else do
                    let step' = updateStep step
                    pure (step', (canOre, canCla, canObs))

        -- dfs until max time, keep best geos
        go !best !step coulds = if step.time == maxT
            then max best step.geos.num
            else
                let next = nextStates best step coulds
                in foldl' (\b (step', coulds') -> max b (go b step' coulds')) best next

p1 :: String -> Int
p1 = sum . zipWith (*) [1..] . map (simulate 24) . parse

p2 :: String -> Int
p2 = product . map (simulate 32) . take 3 . parse
