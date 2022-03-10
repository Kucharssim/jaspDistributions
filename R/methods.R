# pdf ----
pdf <- function(distribution, x, log = FALSE) {
  UseMethod("pdf")
}

#' @export
pdf.jaspDistribution <- function(distribution, x, log = FALSE) {
  args          <- transform(distribution[["parameters"]])
  args[["x"]]   <- x
  args[["log"]] <- log

  return(do.call(distribution[["functions"]][["pdf"]], args))
}

# cdf ----
cdf <- function(distribution, q, lower.tail = TRUE, log.p = FALSE) {
  UseMethod("cdf")
}

#' @export
cdf.jaspDistribution <- function(distribution, q, lower.tail = TRUE, log.p = FALSE) {
  args                 <- transform(distribution[["parameters"]])
  args[["q"]]          <- q
  args[["lower.tail"]] <- lower.tail
  args[["log.p"]]      <- log.p

  return(do.call(distribution[["functions"]][["cdf"]], args))
}

# qf ----
qf <- function(distribution, q, lower.tail = TRUE, log.p = FALSE) {
  UseMethod("qf")
}

#' @export
qf.jaspContinuousDistribution <- function(distribution, p, lower.tail = TRUE, log.p = FALSE) {
  args                 <- transform(distribution[["parameters"]])
  args[["p"]]          <- p
  args[["lower.tail"]] <- lower.tail
  args[["log.p"]]      <- log.p

  return(do.call(distribution[["functions"]][["qf"]], args))
}

# rng ----
rng <- function(distribution, n) {
  UseMethod("rng")
}

#' @export
rng.jaspDistribution <- function(distribution, n) {
  args <- transform(distribution[["parameters"]])
  args[["n"]] <- n

  return(do.call(distribution[["functions"]][["rng"]], args))
}




# simple normal constructor ----
normal <- function(mu, sigma, sigma2, tau, kappa) {
  parametrization <- rlang::check_exclusive(sigma, sigma2, tau, kappa)


  result <- list()

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

# parameters ----
pr <- function(value, label, lower = -Inf, upper = Inf, fixed = FALSE) {
  attr(value, "name")  <- paste(deparse(substitute(value), 500), collapse = "\n")
  attr(value, "label") <- label
  attr(value, "lower") <- lower
  attr(value, "upper") <- upper
  attr(value, "fixed") <- fixed

  class(value) <- "jaspParameter"
  return(value)
}

#' @export
print.jaspParameter <- function(parameter) {
  cat(attr(parameter, "name"), "\n")
  cat(attr(parameter, "label"), "=", parameter, "\n")
  cat("fixed:", attr(parameter, "fixed"), "\n")
  cat("lower:", attr(parameter, "lower"), "\n")
  cat("upper:", attr(parameter, "upper"), "\n\n")
}

pars <- function(..., transformations = NULL) {
  parameters <- list(...)
  names(parameters) <- vapply(parameters, \(p) attr(p, "name"), character(1))
  attr(parameters, "transformations") <- transformations

  class(parameters) <- "jaspParameters"
  return(parameters)
}

#' @export
print.jaspParameters <- function(parameters) {
  cat("Parameters\n")
  cat("==========\n")
  lapply(parameters, print)

  transformations <- attr(parameters, "transformations")
  if(!is.null(transformations)) {
    cat("Transformations\n")
    print(data.frame(definition = attr(parameters, "transformations"), value = unlist(transform(parameters))))
    cat("\n")
  }
}

#' @export
transform.jaspParameters <- function(parameters) {
  transformations <- attr(parameters, "transformations")
  if(is.null(transformations)) return(transformations)

  env <- list2env(parameters)
  result <- lapply(transformations, function(transformation) {
    par <- eval(parse(text = transformation), envir = env)
    par <- as.numeric(par)
    return(par)
  })
  names(result) <- names(transformations)

  return(result)
}
