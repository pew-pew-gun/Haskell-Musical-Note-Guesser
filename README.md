# Haskell-Musical-Note-Guesser

Musician is a 2-player logical guessing game. One player is the composer and the other is the performer/guesser, this code will implement both players.

The composer will select a three-pitch musical chord, where each pitch comprises a musical note, one of A, B, C, D, E, F, or G, and an octave, one of 1, 2, or 3. This chord will be the target for the game. 
The order of pitches in the target is irrelevant, and no pitch may appear more than once. This game does not include sharps or flats, and no more or less than three notes may be included in the target.

The performer will then repeatedly choose a guess chord and for each guess by the performer, the composer will return feedback in the form:

- How many pitches in the guess are included in the target (correct pitches)
- How many pitches have the right note but the wrong octave
- How many pitches have the right octave but the wrong note

Correct pitches are not also counted as correct notes and octaves. 
For example, with a target of A1, B2, A3, a guess of A1, A2, B1 would be counted 
as 1 correct pitch (A1), two correct notes (A2, B1) and one correct octave (A2).

Using this feedback, the performer will continually generate a new guess until
it has successfully guessed the target chord.

The goal of the game is for the performer to guess the random 3-pitch musical chord (the target) chosen by the composer with as few guesses as possible.

The Main.hs file contains the driver function: 'main' (without quotation marks) that will use the code in proj2.hs to make guesses continually until the correct chord is found, showing all guesses and the number of guesses taken. This code was generously provided by Peter Schachte of the University of Melbourne. To run the testing code, just give the main command at the prompt, and enter the target you would like to use. An example is shown below:

*Main> main
Target chord (3 pitches separated by spaces): G2 A2 F2
Your guess #1:  [A1,B1,C2]
    My answer:  (0,1,1)
Your guess #2:  [C1,D3,E3]
    My answer:  (0,0,0)
Your guess #3:  [A2,F2,G2]
    My answer:  (3,0,0)
You got it in 3 guesses!
