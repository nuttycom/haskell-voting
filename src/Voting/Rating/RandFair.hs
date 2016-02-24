module Voting.Rating.RandFair where

import Control.Lens
import Control.Lens.Tuple
import Control.Monad.Random
import Control.Monad.State

import Data.Foldable (foldl', find)
import Data.List (sortOn)
import Data.Maybe (fromJust)
import Data.Monoid (mempty, mappend)
import qualified Data.Map as M
import Voting.Rating.Types

countVotes :: (Foldable f, Ord a, Monoid b, Ord b, Random b, MonadRandom m) => f (Ballot a b) -> m a
countVotes ballots = 
  let appendBallot acc ballot = M.unionWith mappend (ratings ballot) acc
      totals = foldl' appendBallot M.empty ballots
      winRanges = ranges $ sortOn snd (M.toList totals)
      rangeMax =  last winRanges ^. _3
      inRange i (a, lower, upper) = lower <= i && i < upper
  in  do
    winIndex <- getRandomR (mempty, rangeMax)
    pure . (^. _1) . fromJust $ find (inRange winIndex) winRanges 

ranges :: (Monoid b) => [(a, b)] -> [(a, b, b)]
ranges xs = evalState (traverse accum xs) mempty

accum :: (Monoid b) => (a, b) -> State b (a, b, b)
accum (a, total) = do
  lower <- get
  modify $ mappend total 
  (,,) a lower <$> get
