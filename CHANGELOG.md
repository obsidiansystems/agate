# Revision history for agate

## 0.2.0.0 -- 2026-03-18

### Stochastic simulation
* Stochastic interpretation of Petri nets via the Gillespie algorithm
* Markov kernel abstraction using LazyPPL
* Convergence testing against deterministic ODE solutions
* Verification of P-invariants for stochastic trajectories

### General ODE systems
* Real-valued ODE system type class, not restricted to polynomials
* Double pendulum and Rossler attractor examples

## 0.1.0.0

### Petri nets
* Core Petri net representation using sets for places and transitions
* Mass action semantics for deriving ODE systems from Petri nets
* GraphViz-based diagram generation with configurable layout options
* Animated SVG diagrams showing population dynamics over time
* Epidemiological model examples: SIR, SIS, SIRS, SIRD, SEIR, SEAIR,
  SIWR, Malthusian, and Lotka-Volterra

### ODE solving
* Polynomial ODE systems with mass action translation from Petri nets
* Euler method solver
* Stacked area charts and animated diagrams for visualising solutions

### Oriented graded posets
* OG-poset representation and validation
* Enumeration of closed subsets
* Verification of algebraic identities (Lemma 16, formula 14.2)

### Idealized Algol
* Basic interpreter for an idealized Algol-like language
