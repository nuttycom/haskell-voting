{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Voting.Ranked.IRV where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State

import Data.Foldable (minimumBy, toList, find, foldl')
import Data.Ord (comparing)
import Data.Monoid 

import qualified Data.Map as M
import Voting.Ranked.Types

data BallotError a = EmptyBallotList
                   | NoWinner

data S a = S 
  { voteCounts :: M.Map a Int
  , groupedBallots :: M.Map a [Ballot a]
  }

emptyS :: forall a. S a 
emptyS = S M.empty M.empty

countVotes :: (Foldable f, Ord a, MonadError (BallotError a) m) => f (Ballot a) -> m a
countVotes ballots = 
  if null ballots
    then throwError EmptyBallotList
    else maybe (throwError NoWinner) pure $ findWinner (appendBallots emptyS ballots)

findWinner :: forall a. (Ord a) => S a -> Maybe a
findWinner (s@(S counts grouped)) = 
  let totalVotes = getSum $ foldMap Sum counts 
      winner = fmap fst $ find (\x -> toRational (snd x) / toRational totalVotes > 0.5) (M.toList counts)
      loser = fst $ minimumBy (comparing snd) $ M.toList counts
      toReallocate = M.lookup loser grouped
      s' = fmap (removeLoser loser . appendBallots s) toReallocate
  in  winner <|> (findWinner =<< s')
      
appendBallots :: (Ord a, Foldable f) => S a -> f (Ballot a) -> S a
appendBallots s ballots = 
  foldl' appendBallot s ballots

removeLoser :: (Ord a) => a -> S a -> S a
removeLoser loser (S counts grouped) = 
  S (M.delete loser counts) (M.delete loser grouped)

appendBallot :: (Ord a) => S a -> Ballot a -> S a
appendBallot (S counts grouped) (Ballot (x : xs)) = 
  S (M.adjust (+1) x counts) (M.adjust (Ballot xs :) x grouped)
