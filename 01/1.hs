import Data.Sequence ( Seq, fromList, unstableSort )
import Data.Foldable ( toList, find )
import Data.Maybe ( isJust, fromJust )

target = 2020

-- Build a staircase of tuple lists, that is:
-- given [ a, b, c, d, ... ]
-- produce
-- [[ (a, b)  , [ (b, c)  , [ (c, d)  , ... ]
--    (a, c)      (b, d)      ...    ]
--    (a, d)      ...    ]
--    ...    ]
buildStaircase (x:xs) = (zip xs $ cycle [x]) : buildStaircase xs

main = do
  entries <- getContents
  let
    -- read input list and sort it in O(nlogn)
    snumbers = toList $ unstableSort $ fromList $
               map read $ lines entries :: [Int]
    -- build the staircase
    combinations = buildStaircase snumbers
    -- shorten the tuple lists, cutting off tuples where fst * snd > target
    shorties = map (takeWhile (\(f, s) -> f + s <= target)) combinations
    -- transform the tuple lists into the first tuple they contain so that
    -- fst * snd = target
    candidates = map (find (\(f, s) -> f + s == target)) shorties
  -- calculate the result from the first tuple satisfying the criterion
  putStrLn $ show $ uncurry (*) $ fromJust $ head $ filter (isJust) candidates
