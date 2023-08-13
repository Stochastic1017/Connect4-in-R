# Connect four in Rstudio

The repository contains R code to implement and play a functional connect four game. The codes accomplish the following tasks:
* Create and visualize a 6-by-7 grid in the console.
* Check win status, horizontal, vertical, left diagonal, and right diagonal. If won, code also highlights the markers that connect four. If not, code shows a draw.
* Functional playing loop against the computer that plays a random playable move. No AI has been implemented.

The code can be improved in many ways to make the connect 4 game more computationally efficient and fun, some examples include (but not limited to):
* More efficient ways of appending and checking for combinations of playable moves, compared to brute-forcing it. As the board gets filled up, the computer takes a much longer time to find a random playable move.
* Implement an AI algorithm to play as the opponent. Examples include Mini-max algorithms, Alpha-beta pruning algorithm, Utility-function heuristic evaluator, Monte-Carlo Tree Search, Long-Short Term Memory models, etc. Strategies could involve attacking, defending, or adjusting according to player skill.

The codes are open-source and free to use for any and all purposes.
