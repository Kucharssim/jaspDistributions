#' @export
mle.jaspDistribution <- function(data, distribution) {
  pars <- free(distribution$parameters)
  if (length(pars) == 0) return(distribution)

  objective <- function(pars) {
    setPars(distribution, pars)
    likelihood(distribution, data, log = TRUE, scaling = -1)
  }

  o <- stats::optim(
    par = value(pars), lower = lower(pars), upper = upper(pars),
    fn = objective,
    method = "L-BFGS-B")
}


