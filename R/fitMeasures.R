#' @name fit
#' @rdname fit
#'
#' @title Fit measures of distributions
#'
#' @description
#' Measures of absolute and relative fit of a distribution to the data
#'
#' @param distribution Object of class `jaspDistribution`.
#' @param data Data.
#' @param estimated Logical (default `FALSE`): are parameters of the distribution estimated using the same data set as the one used for computing the fit measure?
#'
NULL

#' @rdname fit
#' @export
AIC.jaspDistribution <- function(distribution, data) {
  data <- na.omit(data)
  lik <- likelihood(distribution, data, log = TRUE, scaling = -2)
  npar <- free(distribution$parameters) |> length()

  return(2 * npar - lik)
}

#' @rdname fit
#' @export
BIC.jaspDistribution <- function(distribution, data) {
  data <- na.omit(data)
  lik <- likelihood(distribution, data, log = TRUE, scaling = -2)
  npar <- free(distribution$parameters) |> length()

  return(log(length(data)) * npar - lik)
}

#' @rdname fit
#' @export
andersonDarling <- function(distribution, data, estimated = FALSE) {
  result <- goftest::ad.test(data, null = cdf, distribution = distribution, estimated = estimated)
  return(data.frame(test="ad", statistic=unname(result$statistic), pvalue=result$p.value))
}

#' @rdname fit
#' @export
cramerVonMises <- function(distribution, data, estimated = FALSE) {
  result <- goftest::cvm.test(data, null = cdf, distribution = distribution, estimated = estimated)
  return(data.frame(test="cvm", statistic=unname(result$statistic), pvalue=result$p.value))
}

#' @rdname fit
#' @export
kolmogorovSmirnov <- function(distribution, data) {
  result <- stats::ks.test(x = data, y = cdf, distribution = distribution, alternative = "two.sided")
  return(data.frame(test="ks", statistic=unname(result$statistic), pvalue=result$p.value))
}

#' @rdname fit
#' @export
fitMeasures <- function(distribution, data, estimated = FALSE) {
  UseMethod("fitMeasures")
}

#' @export
fitMeasures.jaspDistribution <- function(distribution, data, estimated = FALSE) {
  ad  <- andersonDarling  (distribution, data, estimated)
  cvm <- cramerVonMises   (distribution, data, estimated)
  ks  <- kolmogorovSmirnov(distribution, data)
  result <- list(
    absolute = rbind(ad, cvm, ks) |> as.data.frame(),
    relative = c(aic = AIC(distribution, data), bic = BIC(distribution, data))
  )
  return(result)
}
