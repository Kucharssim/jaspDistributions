#' @name sets
#' @rdname sets
#'
#' @title Mathematical sets
#'
#' @description
#' Selection of mathematical sets used for describing a support of distributions and parameters.
#'
#' @param lower Lower bound of the set.
#' @param upper Upper bound of the set.
#' @param inclusive A character vector indicating whether and which of the bounds are included in the set.
#' @param set A set object.
#' @param x Values to check.
#' @param symbol A symbol representing the set object.
#' @param mode Should the set be summarized as `unicode` (nice for working in R console) or as a `latex` expression (nice for JASP).
#'
#' @keywords internal
NULL

#' @rdname sets
#' @export
real <- function(lower=-Inf, upper=Inf, inclusive = c("lower", "upper")) {
  stopifnot(lower < upper)
  out <- list(lower=lower, upper=upper, inclusive = inclusive, unicode = "\u0052", latex = "\\mathbb{R}")
  class(out) <- c("real", "set")
  return(out)
}

#' @rdname sets
#' @export
integers <- function(lower=-Inf, upper=Inf, inclusive = c("lower", "upper")) {
  stopifnot(lower < upper)
  out <- list(lower=lower, upper=upper, inclusive = inclusive, unicode = "\u005A", latex = "\\mathbb{Z}")
  class(out) <- c("integers", "set")
  return(out)
}

#' @rdname sets
#' @export
contains <- function(set, x) {
  UseMethod("contains")
}

#' @export
contains.real <- function(set, x) {
  out <- sapply(x, function(xx) {
    if (xx < set$lower) {
      return(FALSE)
    } else if (xx == set$lower) {
      if ("lower" %in% set$inclusive) return(TRUE) else return(FALSE)
    } else if (xx < set$upper) {
      return(TRUE)
    } else if (xx == set$upper) {
      if ("upper" %in% set$inclusive) return(TRUE) else return(FALSE)
    } else {
      return(FALSE)
    }
  })

  return(out)
}

#' @export
contains.integers <- function(set, x) {
  if(!rlang::is_integerish(x)) {
    return(rep(FALSE, length(x)))
  } else {
    return(contains.real(set, x))
  }
}


#' @rdname sets
#' @export
summary.set <- function(set, symbol="x", mode=c("unicode", "latex")) {
  mode <- match.arg(mode)

  if (mode == "latex") {
    main <- sprintf("%s \\in %s", symbol, set$latex)
    lb <- if("lower" %in% set$inclusive) "<" else "\\leq"
    ub <- if("upper" %in% set$inclusive) "<" else "\\leq"
  } else {
    main <- sprintf("%s \u2208 %s", symbol, set$unicode)
    lb <- if("lower" %in% set$inclusive) "\u003C" else "\u2264"
    ub <- if("upper" %in% set$inclusive) "\u003C" else "\u2264"
  }

  if (is.infinite(set$lower) && is.infinite(set$upper)) {
    out <- main
  } else if (is.infinite(set$lower) && !is.infinite(set$upper)) {
    out <- sprintf("%s: %s %s %s", main, symbol, ub, set$upper)
  } else if (!is.infinite(set$lower) && is.infinite(set$upper)) {
    out <- sprintf("%s: %s %s %s", main, set$lower, ub, symbol)
  } else {
    out <- sprintf("%s: %s %s %s %s %s", main, set$lower, lb, symbol, ub, set$upper)
  }
  return(out)
}

#' @rdname sets
#' @export
print.set <- function(set, symbol="x") {
  cat(summary(set, symbol))
}
