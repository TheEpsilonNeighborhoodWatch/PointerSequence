import Sequence
import System.IO

data SeqError = AssignedN | Contradiction deriving (Show, Eq)

fill :: Sequence -> Integer -> Either SeqError Sequence
fill seq ind
    | not $ isAssigned el                    = Right seq
    | isSatisfied el                         = Right seq
    | isAssigned prevEl && isAssigned nextEl && value prevEl == value nextEl = Right postSeq
    | isAssigned prevEl && isAssigned nextEl = Left Contradiction
    | isAssigned nextEl                      = Right $ setElement postSeq prevInd $ value nextEl
    | isAssigned prevEl && value prevEl == n = Left AssignedN
    | isAssigned prevEl                      = Right $ setElement postSeq nextInd $ value prevEl
    | otherwise                              = Right seq
    where nextInd = ind - 1
          prevInd = ind + value el
          el      = getElement seq $ ind
          prevEl  = getElement seq $ prevInd
          nextEl  = getElement seq $ nextInd
          n       = value $ getElement seq 0
          postSeq = satisfy seq ind

subStep :: Integer -> Integer -> Either SeqError Sequence -> Either SeqError Sequence
subStep _ _ seq@(Left _) = seq
subStep start end (Right seq)
    | start == end = Right seq
    | otherwise    = subStep (start + 1) end  (fill seq start)

step :: Sequence -> Either SeqError Sequence
step seq = subStep (minAssIndex seq) (maxAssIndex seq) (Right seq)

subRun :: Integer -> Either SeqError Sequence -> Either SeqError (Sequence, Bool)
subRun _ (Left a)    = Left a
subRun 100 (Right seq)   = Right (seq, Right seq /= step seq)
subRun count (Right seq) = subRun (count + 1) (step seq)

run :: Sequence -> Either SeqError (Sequence, Bool)
run seq = subRun 0 (Right seq)

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

ranPartitions :: [[[Integer]]] -> [(Bool, [Integer])]
ranPartitions previouses = [ (b,l) | (Right (_,b), l) <- [(run $ makeSequence x, x)| x <- concat $ zipWith (map.(:)) [1..] previouses]]
 
startPartition :: [[[Integer]]]
startPartition = [[[]]]

testForever :: [[[Integer]]] -> Handle -> Handle -> IO ()
testForever part state cEx = do
    let ran = ranPartitions part
    let newPart = map snd ran
    hPrint state $ newPart
    hFlush state
    hPrint cEx $ [ l | (True, l) <- ran]
    hFlush cEx
    testForever (newPart:part) state cEx

main :: IO ()
main = withFile "savedstate.txt" AppendMode $ withFile "counter_ex.txt" AppendMode . (testForever startPartition)

