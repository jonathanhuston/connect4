# connect4

Connect4 in Clojure using minimax and alpha-beta pruning

## Usage

    $ java -jar connect4-0.1.0-standalone.jar [- X O] [0 1 2 3 4 5 6 7 8 9] [-s]

## Options

Player:  – (computer vs. computer), X (human first), O (computer first)

Depth:   0 to 9

Flag:    -s displays scores for all possible moves

Default: computer vs. computer at search depth 3, no score info
