{-|
Author  : Bi Ho Shin (1086159)
Email   : bihos@student.unimelb.edu.au
Purpose : Plays a 2-player logical guessing game of Musician. This code 
          implements both the composer and the performer. 
          
Description:
Goal of the game is for the performer to guess the random 3-pitch musical chord 
(the target) chosen by the composer with as few guesses as possible.

Each pitch is comprised of a musical note (one of A, B, C, D, E, F, or G) 
and an octave (one of 1, 2, or 3). 

For each guess by the performer, the composer will return feedback in the form:

- How many pitches in the guess are included in the target (correct pitches)
- How many pitches have the right note but the wrong octave
- How many pitches have the right octave but the wrong note

Correct pitches are not also counted as correct notes and octaves. 
For example, with a target of A1, B2, A3, a guess of A1, A2, B1 would be counted 
as 1 correct pitch (A1), two correct notes (A2, B1) and one correct octave (A2).

Using this feedback, the performer will continually generate a new guess until
it has successfully guessed the target chord.
-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Proj2 (Pitch, toPitch, feedback,
              GameState, initialGuess, nextGuess) where

import Data.List
import Data.Maybe
import Data.Ord


-- |An enumeration of possible musical notes
data Note = A | B | C | D | E | F | G 
    deriving (Eq, Ord, Show, Read) 

-- |An enumeration of only 3 octaves that we will be working with
data Octave = Oct1 | Oct2 | Oct3
    deriving (Eq, Ord, Show, Read) 

data Pitch = Pitch { note :: Note
                   , octave :: Octave
                   } deriving Eq
-- |show redefined to return only the note and numerical octave as a string
instance Show Pitch where
    show (Pitch note oct) = show note ++ [last (show oct)]


type Chord = [Pitch] -- For our game, we only deal with 3-pitch chords
type GameState = [Chord] -- A list of possible candidate target chords


{-|
    Gives just the pitch named by the input String, or Nothing if the string is 
    not a valid pitch name.
-}
toPitch :: String -> Maybe Pitch
toPitch []  = Nothing
toPitch [x] = Nothing
toPitch (note:oct:empt) 
  = if validPitch (note:oct:empt)
    then Just $ Pitch (read [note] :: Note) (read ("Oct" ++ [oct]) :: Octave)
    else Nothing

{-|
    Will take in a case-sensitive String that represents a pitch and will 
    determine if it is a valid pitch.
-}
validPitch :: String -> Bool
validPitch []  = False
validPitch [x] = False
validPitch (note:oct:empt) 
  = let notes = "ABCDEFG"
        octaves = "123"
    in length (note:oct:empt) == 2 
        && elem note notes
        && elem oct octaves
        && null empt

{-|
    Takes a target and guess chord, respectively, and returns the appropriate
    feedback in a 3-tuple format: (correctPitches, correctNotes, correctOctaves)
-}
feedback :: [Pitch] -> [Pitch] -> (Int, Int, Int)
feedback target guess = (correctPitches, correctNotes, correctOctaves)
    where correctPitches = (length target) - length (target \\ guess)
          -- correct pitches are not also counted as correct notes and octaves
          targetLessDups = target \\ guess
          guessLessDups  = guess \\ target
          -- Extract the notes
          targetNotes    = map note targetLessDups
          guessNotes     = map note guessLessDups
          -- Extract the octaves
          targetOctaves  = map octave targetLessDups
          guessOctaves   = map octave guessLessDups
          -- Examine the set difference
          correctNotes   = (length targetNotes) 
                            - length (targetNotes \\ guessNotes)
          correctOctaves = (length targetOctaves) 
                            - length (targetOctaves \\ guessOctaves)

{-|
    Takes no input arguments and returns a pair of an initial guess chord and 
    a list of all candidate target chords.
    The initial guess chord is A2 B1 C1, and was chosen based on empirical 
    testing to determine which chord gives the smallest expected number of
    remaining possible targets when tested against every possible valid chord.
-}
initialGuess :: ([Pitch], GameState)
initialGuess = (fstGuess, curState)
    where allPitch = [ fromJust (toPitch [note,oct]) | note <- ['A'..'G'],
                                                       oct <- ['1'..'3']]
          allChord = [ chord | chord <- subsequences allPitch,
                               length chord == 3]
          fstGuess = [ fromJust (toPitch "A2"),
                       fromJust (toPitch "B1"), 
                       fromJust (toPitch "C1")]
          curState = allChord \\ [fstGuess]

{- |
    Takes in a previous guess and GameState, and the feedback from that guess 
    to generate a new guess and updated GameState.
    Removes all possible targets that are inconsistent with the given feedback
    and updates the GameState accordingly.
-}
nextGuess :: ([Pitch], GameState) -> (Int, Int, Int) -> ([Pitch], GameState)
nextGuess (prevGuess, prevState) prevFeedback = (newGuess, newState)
    where newPosTargets = [ posTarget 
                          | posTarget <- prevState 
                          , feedback posTarget prevGuess == prevFeedback]
          newGuess = bestGuess newPosTargets
          newState = newPosTargets \\ [newGuess]

{-|
    Determines the best guess chord given a list of possible target chords.

    Idea is to choose the chord that will produce the lowest expected number 
    of remaining possible targets. So when we further refine the list of 
    possible targets it is the most likely to remove the most target candidates.

    Take a possible target that remains and determine the feedbacks when 
    operated with every other possible target. Repeat this for every other 
    target candidate. Use this information to determine the chord that gives
    the lowest expected number of remaining candidates.
-}
bestGuess :: GameState -> [Pitch]
bestGuess posTargets
  = fst $ minimumBy (comparing snd)
                    (zip posTargets (map (expectation posTargets) posTargets))
                    -- zip makes (chord, expected no of remaining targets) pairs

{-|
    Finds the expected number of remaining targets, for a given list of 
    remaining possible targets, for a given guess chord in the second argument.
-}
expectation :: Fractional a => GameState -> [Pitch] -> a
expectation posTargets curGuess 
    -- mappedFeedbacks is a list of feedback 3-tuples
    = let mappedFeedbacks = map (feedback curGuess)
                                (posTargets \\ [curGuess])
      in expectedNumTargets mappedFeedbacks

{-|
    Determines the feedback between a guess and every possible target in a 
    list of targets, creating a list of feedback 3-tuples.
    Ensures that the guess is not already in the list of targets.
-}
mapFeedback :: [Pitch] -> GameState -> [(Int, Int, Int)]
mapFeedback curGuess posTargets 
    = map (feedback curGuess) (posTargets \\ [curGuess])

{-|
    Takes an unsorted list of possible feedbacks and counts up the number of 
    distinct feedbacks and determines the expected number of remaining targets.
-}
expectedNumTargets :: (Ord a, Fractional b) => [a] -> b
expectedNumTargets xs 
    = sum $ map ((\ x -> fromIntegral (x ^ 2) / total) . snd) (freqs xs)
        where total = fromIntegral $ length xs

{-|
    Builds up a frequency table.

    Takes as input an unsorted list of objects and returns another list of 
    (object, integer) pairs, where the first argument is a distinct element from
    the original list and the second argument represents the number
    of times that distinct object occurs.
-}
freqs :: Ord a => [a] -> [(a, Int)]
freqs xs = zip sortedXs (map length (group sortedXs))
    where sortedXs = sort xs