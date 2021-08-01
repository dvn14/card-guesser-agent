------------------------------------------------------------------------------
-- AUTHOR
--
-- Devin Nanayakkara
-- Master of Data Science, University of Melbourne
--
-- Purpose: implementation of a card guessing agent using Haskell

------------------------------------------------------------------------------
-- THE GAME
--
-- Two players face each other, each with a complete standard deck of western
-- playing cards (without jokers). One is the answerer and the other is the
-- guesser. The answerer begins by selecting some number of cards from their
-- deck without showing the guesser.
--
-- The aim of the game is for the guesser to guess the answerer's cards.
--
-- In each round, the guesser chooses the same number of cards from their deck
-- to form the guess. The answerer responds with five numbers as feedback
-- for the guess:
--    1. Correct Cards: how many cards in the answer are also in the guess.
--    2. Lower Ranks: how many cards in the answer have rank lower than the
--       lowest rank in the guess.
--    3. Correct Ranks: how many cards in the answer have the same rank as a
--       card in the guess. Note: each card in the guess is only counted once.
--    4. Higher Ranks: how many cards in the answer have rank higher than the
--       highest rank in the guess.
--    5. Correct Suits: how many cards in the answer have the same suit as a
--       card in the guess. Note: each card in the guess is only counted once.
-- This process is repeated until the guesser guesses the answer correctly.

------------------------------------------------------------------------------
-- ASSUMPTIONS
--
-- 1. Cards cannot be repeated in either answer or guess.
-- 2. Order of the cards in the answer and the guess is immaterial.
-- 3. Number of cards picked will be between 2 and 4 (inclusive).

------------------------------------------------------------------------------
-- LIBRARIES
--
-- This program uses Prelude: standard module, and Data.List

------------------------------------------------------------------------------
-- THE PROGRAM

module Proj2 (feedback, initialGuess, nextGuess, GameState) where


-- | Card module provides the Card, Rank, and Suit types and their
--   constructors. Refer to Card.hs for further details.

import Card
import Data.List


-- | The GameState is a list of list of Cards. GameState holds the list of
--   remaining possible card combinations.

type GameState = [[Card]]


-- | feedback function takes a target and a guess in order, and returns the 
--   five number feedback: correct cards, lower ranks, correct ranks, higher
--   ranks and correct suits. The target and guess are list of Cards whilst
--   feedback is a 5 integer tuple.

feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback target guess
    = (correctCards, lowRanks, correctRanks, highRanks, correctSuits)
    where
        correctCards = length $ intersect target guess
        targetRanks  = [r | Card _ r <- target]
        guessRanks   = [r | Card _ r <- guess]
        lowRanks     = length [r | r <- targetRanks, r < minimum guessRanks]
        highRanks    = length [r | r <- targetRanks, r > maximum guessRanks]
        correctRanks = length target - length (targetRanks \\ guessRanks)
        targetSuits  = [s | Card s _ <- target]
        guessSuits   = [s | Card s _ <- guess]
        correctSuits = length target - length (targetSuits \\ guessSuits)


-- | initialGuess function takes the number of cards (in the answer) as
--   input and returns a pair containing the initial guess (as a list of
--   Cards) and the game state. The 'state' contains all possible non-repeated
--   combination of Cards excluding the initial guess.

initialGuess :: Int -> ([Card], GameState)
initialGuess n = (guess, state)
    where
        allCards   = [minBound..maxBound]::[Card]
        allGuesses = getAllGuesses n allCards
        guess      = makeGuess n        
        state      = delete guess allGuesses


-- | makeGuess function takes an integer, n as input and returns n many Cards.
--   This is a helper function for initialGuess which returns the initial guess
--   of cards. The Cards are selected so that the Ranks are equidistant to each
--   other (13/(n+1)) in addition to having different Suits.

makeGuess :: Int -> [Card]
makeGuess n
    | n == 2 = [Card Club R6, Card Heart R10]
    | n == 3 = [Card Club R5, Card Heart R8, Card Spade Queen]
    | n == 4 = [Card Club R3, Card Diamond R6, Card Heart R9, Card Spade Queen]


-- | getAllGuesses function takes number of Cards (size) as integer with a list
--   of Cards, and returns a list of list of Cards where each list is of length
--   size. Moreover, the returned list of lists contains all possible
--   combinations of Cards with given input size. This approach avoids using
--   Data.List function, 'subsequesces' to avoid any infinite loops and time
--   and space complex computations.

getAllGuesses :: Int -> [Card] -> [[Card]]
getAllGuesses 0 _  = [[]]
getAllGuesses _ [] = []
getAllGuesses n cards@(x:xs)
    | n < 0     = []
    | otherwise = 
        case drop (n-1) cards of
            []  -> []
            [_] -> [cards]
            _   -> [x:tl | tl <- getAllGuesses (n-1) xs] ++ getAllGuesses n xs


-- | nextGuess function takes a pair containing the previous guess and game
--   state along with the obtained feedback, and returns the next guess with a
--   new game state. 'newState' contains the pruned game state where only the
--   answers which result in the same score for the previous guess are kept.

nextGuess :: ([Card], GameState) -> (Int,Int,Int,Int,Int) -> ([Card], GameState)
nextGuess (prevGuess, prevState) score
    | length prevGuess < 4 = (newGuess, newState)
    | otherwise            = (newState!!0, newState)
    where
        newState = delete prevGuess [ guess
                                    | guess <- prevState
                                    , feedback guess prevGuess == score]
        newGuess = getBestGuess newState


-- | getBestGuess function takes in the game state and returns the best
--   possible guess, which is a list of Cards. The best guess is obtained such
--   that, when chosen, it will result in the lowest number of remaining 
--   answers, on average. This is determined by applying a utility score
--   function for each answer of the game state. Here lower utility values 
--   indicate greater preference.

getBestGuess :: GameState -> [Card]
getBestGuess state = fst $ head bestGuess
    where
        utilScores = [ (cardSet, utility)
                     | cardSet <- state
                     , let newState = state \\ [cardSet]
                     , let utility  = utilityScore cardSet newState]
        bestGuess  = sortOn snd utilScores


-- | utilityScore function takes a guess and a game state and returns a
--   utility score, where lower utility values indicate greater preference. The
--   score is generated by the average number of possible answers that will
--   remain if the guess is chosen. The aim is to have many small groups,
--   because if G is chosen as a guess, that will probably leave few possible
--   answers when the feedback is given.
--
--   Calculation: Given a candidate guess, get the feedback for each possible
--   answer. Then all answers are grouped by the feeback. Finally, obtain the
--   average of the sizes of these groups, weighted by the sizes of the groups. 

utilityScore :: [Card] -> GameState -> Double
utilityScore possibleGuess state
    = (sum [ nextOutcomes^2
           | fbScore <- groupedFb
           , let nextOutcomes = (fromIntegral . length) fbScore])
      / totalOutcomes
    where
        totalOutcomes = (fromIntegral . length) state
        getFeedback   = [ fb 
                        | cardSet <- state
                        , let fb = feedback possibleGuess cardSet]
        groupedFb     = (group . sort) getFeedback
