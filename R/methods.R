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

#' @export
pdf.jaspTruncatedDistribution <- function(distribution, x, log = FALSE) {
  lpdf  <- pdf.jaspDistribution(distribution, x, log = TRUE)
  lower <- cdf.jaspDistribution(distribution, q = distribution[["lower"]], log.p = FALSE)
  upper <- cdf.jaspDistribution(distribution, q = distribution[["upper"]], log.p = FALSE)
  lnorm <- log(upper - lower)

  lpdf <- lpdf - lnorm

  lpdf <- ifelse(
    x < distribution[["lower"]] | x > distribution[["upper"]], -Inf, lpdf
  )

  if (log) {
    return(lpdf)
  } else {
    return(exp(lpdf))
  }
}

#' @export
pdf.jaspCensoredDistribution <- function(distribution, x, log = FALSE) {
  out <- sapply(x, function(xx) {
    if (xx < distribution[["lower"]]) {
      return(0)
    } else if (xx == distribution[["lower"]]) {
      return(cdf.jaspDistribution(distribution, xx))
    } else if (xx < distribution[["upper"]]) {
      return(pdf.jaspDistribution(distribution, xx))
    } else if (xx == distribution[["upper"]]) {
      return(cdf.jaspDistribution(distribution, xx, lower.tail = FALSE))
    } else {
      return(0)
    }
  })

  if(log)
    out <- log(out)

  return(out)
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

#' @export
cdf.jaspTruncatedDistribution <- function(distribution, q, lower.tail = TRUE, log.p = FALSE) {
  lower <- cdf.jaspDistribution(distribution, q = distribution[["lower"]], lower.tail = TRUE, log.p = FALSE)
  upper <- cdf.jaspDistribution(distribution, q = distribution[["upper"]], lower.tail = TRUE, log.p = FALSE)
  pp <- cdf.jaspDistribution(distribution, q = q, lower.tail = TRUE, log.p = FALSE)

  p <- ifelse(
    pp < lower,
    0, ifelse(
      pp > upper,
      1,
      (pp - lower) / (upper - lower)
    )
  )

  if (!lower.tail) {
    p <- 1-p
  }

  if (log.p) {
    return(log(p))
  } else {
    return(p)
  }
}

#' @export
cdf.jaspCensoredDistribution <- function(distribution, q, lower.tail = TRUE, log.p = FALSE) {
  out <- sapply(q, function(qq) {
    if (qq < distribution[["lower"]]) {
      return(0)
    } else if (qq < distribution[["lower"]]) {
      return(cdf.jaspDistribution(distribution, qq))
    } else {
      return(1)
    }
  })

  if (lower.tail) {
    out <- 1-out
  }

  if (log.p) {
    out <- log(p)
  }

  return(out)
}

# qf ----
qf <- function(distribution, p, lower.tail = TRUE, log.p = FALSE) {
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

#' @export
qf.jaspTruncatedDistribution <- function(distribution, p, lower.tail = TRUE, log.p = FALSE) {
  if (!log.p) {
    p <- log(p)
  }

  if (!lower.tail) {
    p <- log(1-exp(p))
  }

  loss <- function(x, target) {
    p <- cdf(distribution, x, lower.tail = TRUE, log.p = TRUE)
    sqrt((p - target)^2)
  }
  opt <- function(target) {
    result <- try(optimise(f = loss, target = target, lower = distribution[["lower"]], distribution[["upper"]], maximum = FALSE))

    if (jaspBase::isTryError(result)) {
      return(NA)
    } else {
      result$minimum
    }
  }

  q <- sapply(p, opt)

  return(q)
}

#' @export
qf.jaspCensoredDistribution <- function(distribution, p, lower.tail = TRUE, log.p = FALSE) {
  out <- sapply(p, function(pp) {
    qq <- qf.jaspContinuousDistribution(distribution, pp, lower.tail = lower.tail, log.p = log.p)
    if(qq < distribution[["lower"]]) {
      qq <- distribution[["lower"]]
    } else if (qq > distribution[["upper"]]) {
      qq <- distribution[["upper"]]
    }

    return(qq)
  })

  return(out)
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

#' @export
rng.jaspTruncatedDistribution <- function(distribution, n) {
  plower <- cdf.jaspDistribution(distribution, distribution[["lower"]])
  pupper <- cdf.jaspDistribution(distribution, distribution[["upper"]])
  rp <- runif(n, min = plower, max = pupper)
  x <- qf(distribution, rp)
  return(x)
}

#' @export
rng.jaspCensoredDistribution <- function(distribution, n) {
  x <- rng.jaspDistribution(distribution, n)

  x <- sapply(x, function(xx) {
    if (xx < distribution[["lower"]]) {
      return(distribution[["lower"]])
    } else if (xx > distribution[["upper"]]) {
      return(distribution[["upper"]])
    } else {
      return(xx)
    }
  })

  return(x)
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

# printing ----
#' @export
summary.jaspDistribution <- function(distribution, mode = c("unicode", "latex")) {
  out <- list(
    parameters = summary(distribution$parameters, mode = mode),
    support = summary(distribution$support, mode = mode),
    moments = moments(distribution)
  )

  return(out)
}

#' @export
print.jaspDistribution <- function(distribution) {
  header <- paste(distribution[["name"]], "distribution")
  cat(header, "\n")
  cat(rep("=", nchar(header)) |> paste(collapse=""), "\n")
  print(distribution[["parameters"]])

  cat("\n", "Support")
  cat("\n", "-------\n")
  print(distribution[["support"]])

  cat("\n", "Moments")
  cat("\n", "-------\n")
  print(moments(distribution))
}

# parameters ----
fixed <- function(x) {
  attr(x, "fixed") <- TRUE
  x
}

isFixed <- function(x) {
  isTRUE(attr(x, "fixed"))
}

pr <- function(value, name, unicode, latex, support) {
  stopifnot(contains(support, value))
  key <- paste(deparse(substitute(value), 500), collapse = "\n")
  if (missing(name)) {
    name <- key
  }
  if (missing(unicode)) {
    unicode <- name
  }
  if (missing(latex)) {
    latex <- name
  }
  attr(value, "key")     <- key
  attr(value, "name")    <- name
  attr(value, "unicode") <- unicode
  attr(value, "latex")   <- latex
  attr(value, "support") <- support
  attr(value, "fixed")   <- isFixed(value)

  class(value) <- "jaspParameter"
  return(value)
}

#' @export
summary.jaspParameter <- function(parameter, mode = c("unicode", "latex")) {
  attrs <- attributes(parameter)
  mode <- match.arg(mode)
  symbol <- if(mode == "unicode") attrs[["unicode"]] else attrs[["latex"]]

  out <- data.frame(
    name    = attrs[["name"]],
    unicode = attrs[["unicode"]],
    latex   = attrs[["latex"]],
    value   = value(parameter),
    support = attrs[["support"]] |> summary(symbol = symbol, mode = mode),
    fixed   = isFixed(parameter)
  )
  return(out)
}

#' @export
print.jaspParameter <- function(parameter) {
  print(summary(parameter))
}

pars <- function(..., transformations = NULL) {
  parameters <- list(...)
  names(parameters) <- vapply(parameters, \(p) attr(p, "key"), character(1))
  attr(parameters, "transformations") <- transformations

  class(parameters) <- "jaspParameters"
  return(parameters)
}

#' @export
summary.jaspParameters <- function(parameters, mode = c("unicode", "latex")) {
  pars <- lapply(parameters, summary, mode)
  pars <- do.call(rbind, pars)

  transformations <- attr(parameters, "transformations")
  if (!is.null(transformations)) {
    transf <- data.frame(definition = attr(parameters, "transformations"), value = unlist(transform(parameters)))
  } else {
    transf <- NULL
  }
  out <- list(parameters = pars, transformations = transf)

  return(out)
}

#' @export
print.jaspParameters <- function(parameters) {
  x <- summary(parameters)
  cat("","Parameters\n")
  cat("","----------\n")
  print(x$parameters)

  if(!is.null(x$transformations)) {
    cat("\n", "Transformations\n")
    print(x$transformations)
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
truncate <- function(distribution, lower = -Inf, upper = Inf) {
  UseMethod("truncate")
}

#' @export
truncate.jaspContinuousDistribution <- function(distribution, lower = -Inf, upper = Inf) {
  distribution[["lower"]] <- lower
  distribution[["upper"]] <- upper

  class(distribution) <- c("jaspTruncatedDistribution", class(distribution))
  return(distribution)
}

#' @export
censor <- function(distribution, lower = -Inf, upper = Inf) {
  UseMethod("censor")
}

#' @export
censor.jaspContinuousDistribution <- function(distribution, lower = -Inf, upper = Inf) {
  distribution[["lower"]] <- lower
  distribution[["upper"]] <- upper

  class(distribution) <- c("jaspCensoredDistribution", class(distribution))
  return(distribution)
}

#' @export
expectation <- function(distribution) {
  UseMethod("expectation")
}

#' @export
expectation.jaspDistribution <- function(distribution) {
  pars <- transform(distribution[["parameters"]])
  env <- list2env(pars)
  out <- eval(distribution[["expectation"]][["expression"]], envir = env)
  out <- as.numeric(out)
  return(out)
}


#' @export
variance <- function(distribution) {
  UseMethod("variance")
}

#' @export
variance.jaspDistribution <- function(distribution) {
  pars <- transform(distribution[["parameters"]])
  env <- list2env(pars)
  out <- eval(distribution[["variance"]][["expression"]], envir = env)
  out <- as.numeric(out)
  return(out)
}

#' @export
moments <- function(distribution) {
  UseMethod("moments")
}

#' @export
moments.jaspDistribution <- function(distribution) {
  out <- data.frame(
    moment     = c(gettext("Expectation"), gettext("Variance")),
    symbol     = c("\\mathrm{E}(X)", "\\mathrm{Var}(X)"),
    expression = c(distribution[["expectation"]][["latex"]], distribution[["variance"]][["latex"]]),
    value      = c(expectation(distribution), variance(distribution))
    )

  return(out)
}

#' @export
jaspDependencies <- function(distribution) {
  UseMethod("jaspDependencies")
}
