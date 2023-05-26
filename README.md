# WordleGuesser - a Wordle playing program
WordleGuesser uses an entropy-based approach to select the guess that gives the most information about the target word.

To be more specific, each guess we trim down the set of remaining targets to the ones consistent with the feedback we 
received from our last guess. 
A potential guess is then evaluated by calculating all the feedbacks that could be received from the remaining targets. 
We then calulate the entropy of these feedbacks, to provide a score for this potential guess. 
Higher entropy is desirable as it means the feedbacks are more evenly distributed, so the true feedback we receive
contains high information and is likely to cut down the remaining targets as much as possible.


The wordlists are from [here](https://github.com/Kinkelin/WordleCompetition).

* [Wordle.hs](./Wordle.hs) contains the logic for running a Wordle game  
* [WordleGuesser.hs](./WordleGuesser.hs) implements a Guesser to play Wordle

This project was modified from a Declarative Programming (COMP30020) project. 

