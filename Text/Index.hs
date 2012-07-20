{-# LANGUAGE OverloadedStrings #-}

module Text.Index
    ( indexDocument
    , trigrams
    , readtri
    , showtri
    , accumTrigrams
    , countTokens
    , countBigrams
    , countTrigrams
    , Trigram
    , TrigramIndex
    ) where

import           Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import qualified Data.List as L
import           Data.Maybe
import qualified Data.Text as T
import           Import
import           Prelude
import           Text.Tokenizer

type Trigram = (T.Text, T.Text, T.Text)
type TrigramIndex = M.HashMap Trigram Int

indexDocument :: Document -> Maybe Document
indexDocument doc =
    case tokenize (documentContent doc) of
        Left _     -> Nothing
        Right tkns -> Just $ doc { documentTrigrams = Just . trigrams' $ map fst tkns }
    where
        trigrams' tkns = showtri . S.toList . S.fromList $ trigrams tkns

showtri :: [Trigram] -> T.Text
showtri = T.unlines . map (T.unwords . toList)

readtri :: T.Text -> [Trigram]
readtri = catMaybes . map (toTuple . T.words) . T.lines

toList :: (a, a, a) -> [a]
toList (t1, t2, t3) = [t1, t2, t3]

toTuple :: [a] -> Maybe (a, a, a)
toTuple [t1, t2, t3] = Just (t1, t2, t3)
toTuple _            = Nothing

trigrams :: [a] -> [(a, a, a)]
trigrams (a: (as@(b: (c: _)))) = (a, b, c) : trigrams as
trigrams _                     = []

accumTrigrams :: [Document] -> TrigramIndex
accumTrigrams =
    L.foldl' accum M.empty . concat . catMaybes . map (fmap readtri . documentTrigrams)
    where
        accum :: TrigramIndex -> Trigram -> TrigramIndex
        accum m t = M.insertWith (+) t 1 m

set :: (Eq a, Hashable a) => TrigramIndex -> (Trigram -> a) -> S.HashSet a
set index accessor = L.foldl' (flip S.insert) S.empty
                   . map accessor
                   $ M.keys index

countTokens :: TrigramIndex -> Int
countTokens index = S.size $ S.unions [ set index fst3
                                      , set index snd3
                                      , set index trd3
                                      ]

countBigrams :: TrigramIndex -> Int
countBigrams index = S.size $ S.unions [ set index fst2
                                       , set index snd2
                                       ]
    where
        fst2 (a, b, _) = (a, b)
        snd2 (_, a, b) = (a, b)

countTrigrams :: TrigramIndex -> Int
countTrigrams = M.size

