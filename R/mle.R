#' @export
mle <- function(distribution, data, ciLevel = 0.95) {
  UseMethod("mle")
}

#' @export
mle.jaspDistribution <- function(distribution, data, ciLevel = 0.95) {
  pars <- free(distribution$parameters)
  if (length(pars) == 0) {
    warning("Distribution has no free parameters.")
    return(distribution)
  }

  objective <- function(pars) {
    value(distribution$parameters) <- pars
    ll <- likelihood(distribution, data, log = TRUE, scaling = -1)
    return(ll)
  }

  o <- try(stats::optim(
    par = value(pars), lower = lower(pars), upper = upper(pars),
    fn = objective,
    method = "L-BFGS-B", hessian = TRUE))

  if (jaspBase::isTryError(o)) {
    warning("Could not fit the distribution to the data")
    return(distribution)
  }

  class(o) <- "optim"
  value(distribution$parameters) <- o$par
  attr(distribution$parameters, "summary") <- confint.optim(o, level = ciLevel)
  return(distribution)
}


#' @export
confint.optim <- function(x, level = 0.95) {
  vcov <- try(solve(x$hessian))
  if (jaspBase::isTryError(vcov)) {
    stop("Could not invert the hessian matrix")
  }

  results <- lapply(names(x$par), function(par) {
    car::deltaMethod(x$par, par, vcov, level = level)
  })

  results <- do.call(rbind, results)
  colnames(results) <- c("estimate", "se", "lower", "upper")

  return(results)
}
