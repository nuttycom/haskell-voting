{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Voting.Rating.Range where

import Data.Semigroup
import Data.Ord (comparing)
import Data.Foldable (maximumBy, foldl')
import qualified Data.Map as M
import Voting.Rating.Types

countVotes :: (Foldable f, Functor f, Ord a, Semigroup b, Ord b) => f (Ballot a b) -> a
countVotes ballots = 
  let totals = foldl' (M.unionWith (<>)) M.empty $ fmap ratings ballots
      winner = maximumBy (comparing snd) $ M.toList totals
  in  fst winner
