module Sequence (Sequence,
                 makeSequence,
                 isSatisfied,
                 minAssIndex,
                 maxAssIndex,
                 satisfy,
                 isAssigned,
                 value,
                 setElement,
                 getElement) where

import Data.Maybe (isJust)

type Element = (Maybe Integer, Bool)

type Sequence = ([Element], Integer)

makeSequence :: [Integer] -> Sequence
makeSequence ls = (map ((flip (,) $ False) . Just) ls, (+(-1)) $ toInteger $ length ls)

minAssIndex :: Sequence -> Integer
minAssIndex = (0-) . snd

maxAssIndex :: Sequence -> Integer
maxAssIndex = uncurry $ (-) . toInteger . length

uninited = (Nothing, False)

isAssigned :: Element -> Bool
isAssigned = isJust . fst

isSatisfied :: Element -> Bool
isSatisfied = snd

value :: Element -> Integer
value (Just val, _) = val

assign :: Sequence -> Integer -> Element -> Sequence
assign (elms,offset) ind el = (pre ++ el:tail post, offset)
    where (pre,post) = splitAt (fromInteger (ind+offset)) elms

extendLeft :: [Element] -> Integer -> [Element]
extendLeft = flip ((++) . (`replicate` uninited) . fromInteger)
extendRight :: [Element] -> Integer -> [Element]
extendRight = flip (flip (++) . (`replicate` uninited). fromInteger)

setElement :: Sequence -> Integer -> Integer -> Sequence
setElement seq@(elms,offset) ind el
    | offset + ind < 0     = assign ((extendLeft  elms (  -(ind + offset)     )), -ind  ) ind nextEl
    | offset + ind >= len  = assign ((extendRight elms (1 + ind + offset - len)), offset) ind nextEl
    | otherwise            = assign                                  seq                  ind nextEl
    where len    = toInteger $ length elms
          nextEl = (Just el, False)

getElement :: Sequence -> Integer -> Element
getElement (elms,offset) ind
    | offset + ind < 0 = uninited
    | offset + ind >= toInteger (length elms) = uninited
    | otherwise = elms !! fromInteger ( offset + ind )

satisfy :: Sequence -> Integer -> Sequence
satisfy seq@(elms, offset) ind = assign seq ind $ (\(a,_) -> (a,True)) $ elms !! fromInteger (ind+offset)
