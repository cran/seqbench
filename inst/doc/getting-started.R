## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 7, fig.height = 4)

## -----------------------------------------------------------------------------
library(seqbench)

design <- comparison_design(
  alpha   = 0.05,        # P(any false declaration, ever) <= alpha
  margin  = 0.02,        # practical-equivalence margin delta, in loss units
  bounds  = c(0, 1),     # known bounds of the losses of both algorithms
  boundary = "betting",  # Waudby-Smith & Ramdas (2024); the default
  budget  = 800,         # total cost after which the outcome is "inconclusive"
  cost_per_round = 1     # cost of one (instance, seed) evaluation of both algorithms
)
design

## -----------------------------------------------------------------------------
planning_horizon(design, distance = 0.05)             # guaranteed (Hoeffding), very conservative
planning_horizon(design, distance = 0.05, sd = 0.10)  # variance-based approximation

## -----------------------------------------------------------------------------
set.seed(42)
simulate_batch <- function(instances, mu = 0.05, sd = 0.10) {
  h <- sd * sqrt(3)
  la <- runif(length(instances), 0.4, 0.8)
  data.frame(instance = instances, seed = 1L,
             loss_a = la, loss_b = la - mu + runif(length(instances), -h, h))
}

state <- initialize_comparison(design)
state <- update_comparison(state, simulate_batch(1:50))
state

## -----------------------------------------------------------------------------
state <- update_comparison(state, simulate_batch(51:400))
stopping_decision(state)

## -----------------------------------------------------------------------------
comparison_report(state)

## -----------------------------------------------------------------------------
tidy(state)[c(1:3, nrow(tidy(state))), ]
glance(state)

## -----------------------------------------------------------------------------
ggplot2::autoplot(state)

## ----error = TRUE-------------------------------------------------------------
try({
fresh <- initialize_comparison(design)
update_comparison(fresh, data.frame(instance = 1, loss_a = 1.2, loss_b = 0.5))   # outside bounds
update_comparison(fresh, data.frame(instance = 1, loss_a = NA,  loss_b = 0.5))   # missing
})

## -----------------------------------------------------------------------------
tight <- comparison_design(alpha = 0.05, margin = 0.02, bounds = c(0, 1), n_max = 30)
st <- update_comparison(initialize_comparison(tight), simulate_batch(1:30, mu = 0.03))
stopping_decision(st)

