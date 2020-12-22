import Data.Sequence ( Seq, fromList, unstableSort )
import Data.Foldable ( toList, find )
import Data.Maybe ( isJust, fromJust )

target = 2020

-- Build a staircase as in the previous solution, only with triples
buildStaircase (x:xs) = let st = pre xs
                        -- prepend x to all 2-staircases originating from xs
                        -- and concatenate them, forming long, single lists of
                        -- triples that will form the 3-staircase
                        in (concatMap (\s -> zip3 (cycle [x]) (fst s) (snd s))
                            st) : buildStaircase xs
  -- Very similar to the old buildStaircase, but it doesn't zip for convenience
  -- (mapping unzip on the old function achieves the same result)
  where pre (x:xs) = (cycle [x], xs) : pre xs
        pre [] = [([target], [1])]
buildStaircase _ = [[(target, 1, 0)]]

-- This part is very similar to the previous solution, so no comments
main = do
  entries <- getContents
  let
    snumbers = toList $ unstableSort $ fromList $
               map read $ lines entries :: [Int]
    combinations = buildStaircase snumbers
    shorties = map (takeWhile (\(f,s,t) -> f + s + t <= target)) combinations
    candidates = map (find (\(f,s,t) -> f + s + t == target)) shorties
  putStrLn $ show $
    (\(f,s,t) -> f * s * t) $ fromJust $ head $ filter (isJust) candidates
