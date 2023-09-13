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
  result[["parameters"]] <- switch(
    parametrization,
    "sigma"  = pars(pr(mu, "\u03BC"), pr(sigma,  "\u03C3", lower = 0), transformations = c(mean = "mu", sd = "sigma")       ),
    "sigma2" = pars(pr(mu, "\u03BC"), pr(sigma2, "\u03C3", lower = 0), transformations = c(mean = "mu", sd = "sqrt(sigma2)")),
    "tau"    = pars(pr(mu, "\u03BC"), pr(tau,    "\u03C3", lower = 0), transformations = c(mean = "mu", sd = "1/sqrt(tau)") ),
    "kappa"  = pars(pr(mu, "\u03BC"), pr(kappa,  "\u03C3", lower = 0), transformations = c(mean = "mu", sd = "1/kappa")     )
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
