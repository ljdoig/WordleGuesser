# WordleGuesser - a Wordle playing program
WordleGuesser uses an entropy-based approach to select the guess that gives the most information about the target word. 
It averages about 3.46 guesses per word over the full Wordle answer list, successfully guessing all of them. Based on
[this repo](https://github.com/Kinkelin/WordleCompetition) that seems quite good, however that competition doesn't give bots
the full Wordle answer list (only the guess list).

Source files:
* [Wordle.hs](./Wordle.hs) contains the logic for running a Wordle game, either with a bot or a human player  
* [WordleGuesser.hs](./WordleGuesser.hs) implements a Guesser to play Wordle
* [Main.hs](./Main.hs) implements a simple program for users to interact with the WordleGuesser or play Wordle on the command-line


The project has been compiled to a [MacOS executable](./Wordle).

## Functionality
The Main module allows the following options:
* Watch WordleGuesser guess a random Wordle word
* Watch WordleGuesser guess a Wordle word of your choosing
* Compete against WordleGuesser to guess a random Wordle word the fastest
* Use WordleGuesser to cheat (relaying your feedback and optionally using suggestions)
* Play a standard game of Wordle yourself


All except the last showcase the WordleGuesser's abilities.

## The approach in more detail
Each guess we trim down the set of possible guesses to the ones consistent with the feedback we received from our last guess. 
A potential guess is then evaluated by calculating all the feedbacks that could be received from the current possible guesses. 
We then calulate the entropy of these feedbacks, to provide a score for this potential guess. 
Higher entropy is desirable as it means the feedbacks are more evenly distributed, so the true feedback we receive
contains high information and is likely to cut down the remaining targets as much as possible.

This project was modified from a Declarative Programming (COMP30020) project. 

