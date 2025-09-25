-- module Unit.SandhiTest (tests) where

-- import TestCase

-- import Phonology.Sandhi.Boundary
--     (applyBoundarySandhi)


-- data BoundarySandhiTest = BoundarySandhiTest
--     { ruleName :: String
--     , testCategory :: String
--     , inputWords :: (String, String)
--     , testAction :: String
--     , expectedSandhi :: String
--     , expectedTracePrefix :: String
--     } deriving (Show, Eq)

-- prefixMatches :: String -> String -> Bool
-- prefixMatches [] _ = True
-- prefixMatches (currChar : restChars) (currAgainst, restAgainsts)
--     | currChar == currAgainst   = prefixMatches restChars restAgainsts
--     | otherwise                 = False

-- aaV :: [BoundarySandhiResult]
-- aaV = [
--     BoundarySandhiTest "a: V" "word/ac"
--     ("si:ta:", "ashvam") "shouldSandhify"
--     "si:ta:shvam" "word/ac/savarNam",
--     BoundarySandhiTest "a: V" "word/ac"
--     ("si:ta: iSum") "shouldSandhify"
--     "si:teSum" "word/ac/saMhita",]

-- runBoundarySandhiTest :: BoundarySandhiTest -> Spec
-- runBoundarySandhiTest (BoundarySandhiResult name category input action sandhita prefix) =
--     let (inputLeft, inputRight) = input in
--     case action of
--         "shouldSandhify" ->
--             it name $
--                 case applyBoundarySandhi input of
--                     Sandhita resultSandhita resultTrace ->
--                         if resultSandhita == sandhita && resultTrace
--         "shouldNotSandhify" ->
--             it name $ case applyBoundarySandhi input of
--                 Unchanged (resultLeft, resultRight) ->
--                     if resultLeft == inputLeft && resultRight == inputRight then
