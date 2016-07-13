module WangBMachine where

{-
Resources:

-   [Wikipedia](https://en.wikipedia.org/wiki/Turing_machine)
-   The original paper: Turing, A.M. (1936). *On computable numbers,
    with an application to the Entscheidungsproblem*. J. of Math 58,
    5.

There are lots of possible options here. May be worth sticking with
those in the original paper?

Or, even better, implementing the simplest TM possible, and compiling
down to it.

Could adopt the convention that e.g. a non-existent row in the
transition table means HALT. (Yes! Transition function can be
partial.)

Further reading about [Turing machine equivalents]
(https://en.wikipedia.org/wiki/Turing_machine_equivalents) reveals

-  Post's "formulation 1"
-  [**Wang B-machines**](https://en.wikipedia.org/wiki/Wang_B-machine)
   Original paper is Wang, H. (1957). *A Variant to Turing’s Theory of
   Computing Machines*. J. ACM 4, 63–92.
-}


data BInstruction =
    Right | Left | Mark | CondJump Int


data WInstruction =
    Right' | Left' | Mark' | Erase' | CondJump' Int


data TapeState =
    O | X


-- Options: could define an infinite tape lazily then modify; or just
-- construct a finite tape.
data Tape =
    Tape [TapeState] TapeState [TapeState]


blankTape = Tape [] O []
