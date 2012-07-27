{-# LANGUAGE OverloadedStrings #-}

module Text.DissociatedPress
    ( dissociate
    ) where

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Vector as V
import           Prelude
import           System.Random.Mersenne
import           Text.Index (Trigram)

-- | Aliases for each stage of the processing.

type Bigram       = (T.Text, T.Text)
type TokenSet     = S.HashSet T.Text
type TokenVector  = V.Vector T.Text
type BigramIndex' = M.HashMap Bigram TokenSet
type BigramIndex  = M.HashMap Bigram TokenVector
type BigramVector = V.Vector Bigram

-- | This takes a finite stream of trigrams, which should be unique, and
-- returns an infinite stream of outputs, assembled using the [Dissociated
-- Press](http://en.wikipedia.org/wiki/Dissociated_press) algorithm.
--
-- To pre-process the Trigrams, this loads them into a BigramIndex', and once
-- it's built loads it into a BigramIndex. It creates a BigramVector from the
-- BigramIndex's keys. Finally, it uses those to rapidly randomly select items
-- from the indexes.

dissociate :: [Trigram] -> IO [T.Text]
dissociate trigrams = do
    (i0:indexes) <- randoms =<< getStdGen
    let (a, b) = bigvector `ri` i0
        output = a : b : L.zipWith3 (press bigvector bigindex)
                                    output
                                    (L.drop 1 output)
                                    indexes
    return output
    where
        bigindex  = M.map optimizeIndex $ L.foldl' buildBigramIndex' M.empty trigrams
        bigvector = V.fromList $ M.keys bigindex

        buildBigramIndex' :: BigramIndex' -> Trigram -> BigramIndex'
        buildBigramIndex' index (t1, t2, t3) =
            M.insertWith S.union (t1, t2) (S.singleton t3) index

        optimizeIndex :: TokenSet -> TokenVector
        optimizeIndex = V.fromList . S.toList

        ri :: V.Vector a -> Double -> a
        ri v di = (v V.!) . truncate . (di *) . fromIntegral $ V.length v

        press :: BigramVector -> BigramIndex -> T.Text -> T.Text -> Double -> T.Text
        press vector index a b r = maybe rtoken (flip ri r) $ M.lookup bigram index
            where bigram      = (a, b)
                  (rtoken, _) = vector `ri` r

