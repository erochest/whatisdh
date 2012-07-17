{-# LANGUAGE OverloadedStrings #-}

module Text.Index
    ( indexDocument
    , trigrams
    , readtri
    , showtri
    ) where

import qualified Data.HashSet as S
import           Data.Maybe
import qualified Data.Text as T
import           Import
import           Prelude
import           Text.Tokenizer

type Trigram = (T.Text, T.Text, T.Text)

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

