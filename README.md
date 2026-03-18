Agate
=====

A Haskell library for representing and working with dynamical systems,
with a focus on Petri nets and their interpretations as ODEs and
stochastic processes.

## Features

### Petri nets

Petri nets are represented via a type class with a monoidal structure,
where individual transitions are composed together to build larger nets.
Agate provides:

- A concrete implementation using sets for places and transitions
- Mass action semantics for deriving ODE systems from Petri nets
- Incidence matrices and P-invariant computation
- GraphViz-based diagram generation with configurable layout
- Animated SVG diagrams showing population dynamics over time

### ODE systems

A type class for first-order ODE systems, with instances for both
polynomial systems (arising from mass action kinetics) and general
real-valued systems. Includes an Euler method solver and area chart
visualisation.

### Stochastic simulation

Petri nets can also be interpreted stochastically, where transitions
fire according to Poisson-distributed rates (a form of the Gillespie
algorithm). This is built on LazyPPL's probabilistic programming
primitives and includes Markov kernel composition.

### Oriented graded posets

Representation and validation of oriented graded posets, with
enumeration of closed subsets and verification of algebraic identities.

## Examples

The `agate-examples` library component contains a variety of worked
examples, including:

- Epidemiological models: SIR, SIS, SIRS, SIRD, SEIR, SEAIR, SIWR
- Population dynamics: Lotka-Volterra, Malthusian growth
- Physics: double pendulum, Rossler attractor
- General: exponential growth

## Building

Agate uses Cabal:

    cabal build all
    cabal test

## License

BSD-2-Clause. See [LICENSE](LICENSE) for details.
