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

# likelihood ----
likelihood <- function(distribution, x, log = FALSE, scaling = 1) {
  UseMethod("likelihood")
}

#' @export
likelihood.jaspDistribution <- function(distribution, x, log = FALSE, scaling = 1) {
  x <- na.omit(x)
  ll <- pdf(distribution, x, TRUE)
  ll <- scaling * sum(ll[!is.infinite(ll)])
  if (log) {
    return(ll)
  } else {
    return(exp(ll))
  }
}

#' @export
print.jaspDistribution <- function(distribution) {
  cat(distribution[["name"]], " distribution\n")
  print(distribution[["parameters"]])
}
# parameters ----
fixed <- function(x) {
  attr(x, "fixed") <- TRUE
  x
}

isFixed <- function(x) {
  isTRUE(attr(x, "fixed"))
}

pr <- function(value, label, name, lower = -Inf, upper = Inf) {
  if (missing(name)) {
    name <- paste(deparse(substitute(value), 500), collapse = "\n")
  }
  if (missing(label)) {
    label <- name
  }
  attr(value, "name")  <- name
  attr(value, "label") <- label
  attr(value, "lower") <- lower
  attr(value, "upper") <- upper
  attr(value, "fixed") <- isFixed(value)

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

#' @export
value <- function(x, ...) {
  UseMethod("value")
}

#' @export
value.jaspParameter <- function(x) {
  x <- unclass(x)
  attributes(x) <- NULL
  return(x)
}

#' @export
value.jaspParameters <- function(x) {
  x <- sapply(x, value)
  return(x)
}

#' @export
`value<-` <- function(x, value) {
  UseMethod("value<-")
}

#' @export
`value<-.jaspParameter` <- function(parameter, value) {
  # if(value < lower(parameter) || value > upper(parameter)) {
  #   warning("Parameter cannot be assigned a value outside of its support.")
  #   return(parameter)
  # }

  attributes(value) <- attributes(parameter)
  return(value)
}

#' @export
`value<-.jaspParameters` <- function(parameters, values) {
  for (name in names(values)) {
    value(parameters[[name]]) <- values[[name]]
  }

  return(parameters)
}

#' @export
lower <- function(x) {
  UseMethod("lower")
}

#' @export
lower.jaspParameter <- function(x) {
  x <- attr(x, "lower")
  return(x)
}

#' @export
lower.jaspParameters <- function(x) {
  x <- sapply(x, lower)
  return(x)
}

#' @export
upper <- function(x) {
  UseMethod("upper")
}

#' @export
upper.jaspParameter <- function(x) {
  x <- attr(x, "upper")
  return(x)
}

#' @export
upper.jaspParameters <- function(x) {
  x <- sapply(x, upper)
  return(x)
}

#' @export
free <- function(x) {
  UseMethod("free")
}

#' @export
free.jaspParameters <- function(x) {
  fixed <- sapply(x, isFixed)
  x[fixed] <- NULL
  attr(x, "transformations") <- NULL
  return(x)
}


#' @export
summary.jaspParameters <- function(parameters) {
  summary <- attr(parameters, "summary")
  return(summary)
}

#' @export
summary.jaspDistribution <- function(distribution) {
  summary(distribution$parameters)
}
