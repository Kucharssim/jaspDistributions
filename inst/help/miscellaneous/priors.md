Setting up prior distributions
==========================

Prior distributions are needed to set up a Bayesian estimation of parameters. The distributions are specified using the syntax in JAGS (Plummer, 2003).

The following gives an overview of possible distributions to specify, and their parametrizations.

## Unbounded distributions

Unbounded distributions are generally useful for parameters that are unbounded (i.e., can theoretically range from -Inf to Inf)

- `dnorm(mean, precision)`: The normal distribution with the mean and precision parameters.

- `dlogis(location, scale)`: The logistic distribution with the mean and scale parameters.

## Positive distributions

Distributions that are defined only on positive real numbers are helpful for parameters that can be only positive

- `dexp(rate)`: The exponential distribution with the rate parameter.

- `dgamma(shape, rate)`: The gamma distribution with the shape and rate parameters.

- `dchisq(df)`: The chi-squared distribution with a degree of freedom parameter.

## Bounded distributions

Bounded distributions are useful for parameters that are naturally bounded, or in situations where the parameter is not bounded but there is a theoretical expectation that the parameter cannot possible exceed some specific value.

- `dbeta(shape1, shape2)`: The Beta distribution parametrized with shape parameters.

- `dunif(min, max)`: The uniform distribution with the minimum and maximum parameters.

## References

Plummer, M. (2003). JAGS: A Program for Analysis of Bayesian Graphical Models Using Gibbs Sampling. *Proceedings of the 3rd International Workshop on Distributed Statistical Computing.*
