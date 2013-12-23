Common Lisp implementation of a Risk-like board game.

Tentative waypoints
  * Basic API
  * "One-seat" CLI
  * Web frontend?
  * Network server (with CLI?)
  * Network clients
    * CLI?
    * GUI?

NOTES
  * Before first use:
`(quicklisp:quickload "alexandria" "cl-graph")`

DONE
  * Add package definition
  * Add a game class
    * Player repository
    * Territory repositories
    * Frontiers graph; ~~possibly~~ with [cl-graph](http://common-lisp.net/project/cl-graph/)

TODO
  * Turns
    * Polish the FSM a bit more
  * Add continents to game class

