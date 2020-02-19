# connect4

Connect4 in Clojure using minimax and alpha-beta pruning

## Usage

    $ java -jar connect4-0.1.0-standalone.jar [- X O] [0 1 2 3 4 5 6 7 8 9] [-s]

## Options

Player must be – (computer vs. computer), X (human first), O (computer first)

Search depth must be between 0 and 9

Flag -s displays board scores and current alpha/beta

Default (no args): computer vs. computer at search depth 3, no score info
