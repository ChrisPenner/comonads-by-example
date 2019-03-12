## Comonads By Example

This interactive talk series explores the Comonad typeclass while solving real problems. Students are presented with practical examples to ground their understanding and cement their learnings by implementing the things they learn along the way. Git checkpoints are provided so nobody is left behind.  The workshop-style series covers everything from "what's a comonad?" to using comonads for solving real problems, including simulating cellular automata and image processing!

Proposed workshop schedule:

Session 1.  Overview
    -   A high level discussion of what comonads are good for including shallow
        descriptions of a few motivating examples (Zippers, Streams, Conway's
        game of life aka cellular automata, analyzing subtrees, etc)
    -   Explore how the methods of the Comonad typeclass accomplish these
        behaviours and implement some trivial comonads to solve a few simple
        problems (e.g. Identity, Env, NonEmpty list)
Session 2.  Solving problems with 'extend'
    -   Implement the List Zipper comonad together and solve the 'trapped
        rainwater' google interview problem problem using 'extend'.
Session 3.  Store Comonads
    -   Intro to the Store comonad, using pos and seek.
    -   Implement Newton's method to solve square roots using the Store Comonad
Session 4.  Grid-based Comonads, exploring context using 'experiment'
    -   Quick intro to representable functors and
        Control.Comonad.Representable.Store
    -   Implement Cellular Automata via Conway's game of life simulation using
        experiment
    -   Quick exploration of electricity and water flow simulations using
        similar techniques
Session 5.  Image processing using Comonads
    -   Matrices as Representable Store Comonads
    -   Implement a simple sliding window 'Blur' using extend and experiment.
    -   Explore more complex image processing algorithms, e.g. edge detection,
        median colour selection, etc.

Bonus topics:

Day Convolution, Comonads as UIs, Comonads for fluid/electricity flow
simulation, comonads in video games, tree computations using CoFree, and
anything people ask about.
