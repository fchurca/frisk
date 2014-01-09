Common Lisp implementation of a Risk-like board game.

Tentative waypoints
  * Basic API
  * "One-seat" CLI
  * Web frontend? (with [Restas](http://restas.lisper.ru/en/)?)
  * Network server (with CLI?)
  * Network clients
    * CLI?
    * GUI?

USAGE
  * Make sure you have [Quicklisp](http://www.quicklisp.org/beta/) up and running in your favourite CL implementation. I tested it only on SBCL and CLISP, but I think it's portable enough to work *Everywhere™*.
  * Clone the repo inside your local-projects dir:
`cd ~/quicklisp/local-projects/`
`git clone https://github.com/fchurca/frisk.git`
  * Fire up CL and load it via Quicklisp. This will install all required dependencies:
`(quicklisp:quickload 'frisk)`

DONE
  * Add package definition
  * Add a game class
    * Player repository
    * Territory repositories
    * Frontiers graph; ~~possibly~~ with [cl-graph](http://common-lisp.net/project/cl-graph/)

TODO
  * Documentation
  * Check which symbols to export
  * Make FSM a separate project?
  * Turns
    * Polish the FSM a bit more
    * Initial amount of armies per player; save per map per player count?
    * Add victory detection after attacking
  * Add continents to game class

