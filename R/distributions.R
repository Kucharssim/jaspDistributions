#' @name distributions
#' @rdname distributions
#'
#' @title JASP Distributions
NULL

#' @rdname distributions
#' @export
normal <- function(mu, sigma, sigma2, tau, kappa) {
  parametrization <- rlang::check_exclusive(sigma, sigma2, tau, kappa)


  result <- list()
  result[["name"]] <- "Normal"

  par1 <- pr(mu, name = gettext("mean"), unicode = "\u03BC", latex = "\\mu", support = real())
  sp <- real(lower = 0, inclusive = NULL)
  par2 <- switch(
    parametrization,
    "sigma"  = pr(sigma,  name = gettext("standard deviation"),       unicode = "\u03C3",       latex = "\\sigma",   support = sp),
    "sigma2" = pr(sigma2, name = gettext("variance"),                 unicode = "\u03C3\u00B2", latex = "\\sigma^2", support = sp),
    "tau"    = pr(tau,    name = gettext("precision"),                unicode = "\u03C4",       latex = "\\tau",     support = sp),
    "kappa"  = pr(kappa,  name = gettext("square root of precision"), unicode = "\u03BA",       latex = "\\kappa",   support = sp)
  )

  transformations <- switch(
    parametrization,
    "sigma"  = c(mean = "mu", sd = "sigma"),
    "sigma2" = c(mean = "mu", sd = "sqrt(sigma2)"),
    "tau"    = c(mean = "mu", sd = "1/sqrt(tau)"),
    "kappa"  = c(mean = "mu", sd = "1/kappa")
  )

  result[["parameters"]] <- pars(par1, par2, transformations = transformations)
  result[["support"]] <- real()
  result[["expectation"]] <- c(expression=expression(mean), latex="\\mu")
  result[["variance"]] <- switch(
    parametrization,
    "sigma"  = c(expression=expression(sd^2), latex = "\\sigma^2"   ),
    "sigma2" = c(expression=expression(sd^2), latex = "\\sigma^2"   ),
    "tau"    = c(expression=expression(sd^2), latex = "\\tau^{-1}"  ),
    "kappa"  = c(expression=expression(sd^2), latex = "\\kappa^{-2}")
  )
  result[["functions"]] <- list(pdf = dnorm, cdf = pnorm, qf = qnorm, rng = rnorm)

  class(result) <- c("jaspNormal", "jaspContinuousDistribution", "jaspDistribution")
  return(result)
}

#' @rdname distributions
#' @export
exponential <- function(rate, scale) {
  parametrization <- rlang::check_exclusive(rate, scale)

  result <- list()
  result[["name"]] <- "Normal"
  result[["parameters"]] <- switch(
    parametrization,
    "rate"  = pars(pr(rate, lower = 0),  transformations = c(rate = "rate")),
    "scale" = pars(pr(scale, lower = 0), transformations = c(rate = "1/scale"))
  )
  result[["functions"]] <- list(pdf = dexp, cdf = pexp, qf = qexp, rng = rexp)

  class(result) <- c("jaspExponential", "jaspContinuousDistribution", "jaspDistribution")
  return(result)
}

#' @export
mle.jaspExponential <- function(distribution, data, ciLevel = 0.95) {
  cache <- distribution
  alpha <- 1-ciLevel
  pars <- free(distribution$parameters)
  if (length(pars) == 0) {
    warning("Distribution has no free parameters.")
    return(distribution)
  }

  data <- na.omit(data)
  mean <- mean(data)

  if (is.null(distribution$parameter$scale)) {
    value(distribution$parameters$rate) <- mean
  } else {
    value(distribution$parameters$scale) <- 1/mean
  }

  distribution <- try(mle.jaspDistribution(distribution, data, ciLevel))

  if (jaspBase::isTryError(distribution)) {
    return(cache)
  } else {
    return(distribution)
  }
}

#' @rdname distributions
#' @export
binomial <- function(prob, theta, trials) {
  parametrization <- rlang::check_exclusive(prob, theta)


  result <- list()
  result[["name"]] <- "Binomial"
  result[["parameters"]] <- switch(
    parametrization,
    prob = pars(
      pr(value = prob, label = "p", name = "p", lower = 0, upper = 1),
      pr(value = fixed(trials), label = "n", name = "trials", lower = 0),
      transformations = c(prob = "prob", size = "trials")
      ),
    theta = pars(
      pr(value = theta, label = "\u03B8", name = "theta"),
      pr(value = fixed(trials), label = "n", name = "trials", lower = 0),
      transformations = c(prob = "jaspBase::invLogit(theta)", size = "trials")
    )
  )
  result[["functions"]] <- list(pdf = dbinom, cdf = pbinom, qf = qbinom, rng = rbinom)

  class(result) <- c("jaspBinomial", "jaspDiscreteDistribution", "jaspDistribution")
  return(result)
}

#' @rdname distributions
#' @export
poisson <- function(lambda, theta) {
  parametrization <- rlang::check_exclusive(lambda, theta)

  result <- list()
  result[["name"]] <- "Poisson"
  result[["parameters"]] <- switch(
    parametrization,
    lambda = pars(pr(value = lambda, lower = 0), transformations = c(lambda = "lambda")),
    theta = pars(pr(value = theta), transformations = c(lambda = "exp(theta)"))
  )
  result[["functions"]] <- list(pdf = dpois, cdf = ppois, qf = qpois, rng = rpois)

  class(result) <- c("jaspPoisson", "jaspDiscreteDistribution", "jaspDistribution")
  return(result)
}
