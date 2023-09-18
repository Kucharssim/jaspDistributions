#' @name plotDistribution
#' @rdname plotDistribution
#'
#' @title Plots of distributions
#'
#' @description
#' Various plots of probability distribution objects.
#'
#' @param distribution Object of class `jaspDistribution`.
#' @param what Character specifying what kind of plot to display.
#' @param ... Additional arguments passed to the plotting function. Not every argument is used by every function. See details.
#' @param xRange Range of the plot on the x-axis.
#' @param highlightDensity Highlight density at specified points.
#' @param highlightProbability Highlight probability within specified intervals.
#' @param x Data.
#' @param ci Logical (default `FALSE`): Should confidence intervals be displayed?
#' @param ciLevel Level of the confidence interval.
#'
#' @returns A [ggplot2::ggplot()] object.
#'
NULL


# main plotting function ----
#' @rdname plotDistribution
#' @export
plot.jaspDistribution <- function(distribution, what = c("pdf", "cdf", "qf", "histogram", "ecdf", "qq", "pp"), ...) {
  what <- match.arg(what)
  switch(what,
         pdf       = plotPdf (distribution, ...),
         cdf       = plotCdf (distribution, ...),
         qf        = plotCdf (distribution, ...) + ggplot2::coord_flip(),
         histogram = plotHist(distribution, ...),
         ecdf      = plotEcdf(distribution, ...),
         qq        = plotQq  (distribution, ...),
         pp        = plotPp  (distribution, ...))
}

# helper functions ----
plotCurve <- function(data, line = TRUE, lineColor = "black", lineSize = 1.25, shade  = FALSE, shadeColor = "steelblue", shadeAlpha = 1) {
  out <- list()
  if(shade)
    out[[1]] <- ggplot2::geom_ribbon(data = data, mapping = ggplot2::aes(x = x, ymin = 0, ymax = y), inherit.aes = FALSE, fill = shadeColor, alpha = shadeAlpha)
  if(line)
    out[[2]] <- ggplot2::geom_line(data = data, mapping = ggplot2::aes(x = x, y = y), inherit.aes = FALSE, size = lineSize, color = lineColor)

  return(out)
}


prettyFormat <- function(x) {
  ifelse(
    x > 0.001,
    format(round(x, digits = 3)),
    ifelse(
      x == 0,
      "0",
      format(x, digits = 3, scientific = TRUE)
    )
  )
}

# methods ----
plotPdf <- function(distribution, xRange, highlightDensity = FALSE, highlightProbability = FALSE) {
  UseMethod("plotPdf")
}

plotPdf.jaspContinuousDistribution <- function(distribution, xRange, highlightDensity = NULL, highlightProbability = NULL) {
  yRange <- c(0, 0)
  plot <- ggplot2::ggplot()
  if (!is.null(highlightProbability)) {
    for(highlight in highlightProbability) {
      min <- if (highlight[1] < xRange[1]) xRange[1] else highlight[1]
      max <- if (highlight[2] > xRange[2]) xRange[2] else highlight[2]
      x  <- seq(min, max, length.out = 101)
      df <- data.frame(x = x, y = pdf(distribution, x))
      yRange <- range(c(yRange, df$y))
      plotProb <- plotCurve(data = df, line = FALSE, shade = TRUE)
    }
  }

  x  <- seq(xRange[1], xRange[2], length.out = 101)
  df <- data.frame(x = x, y = pdf(distribution, x))
  yRange <- range(c(yRange, df$y))
  plot <- plot + plotCurve(data = df)

  if (!is.null(highlightDensity)) {
    x <- highlightDensity[highlightDensity > xRange[1] & highlightDensity < xRange[2]]
    df <- data.frame(x = x, y = pdf(distribution, x))
    yRange <- range(c(yRange, df$y))
    plot <- plot +
      jaspGraphs::geom_point(data = df, mapping = ggplot2::aes(x = x, y = y))
  }

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(xRange)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(yRange)
  plot <- plot +
    jaspGraphs::themeJaspRaw() +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::scale_x_continuous(limits = range(xBreaks), breaks = xBreaks) +
    jaspGraphs::scale_y_continuous(limits = range(yBreaks), breaks = yBreaks) +
    ggplot2::ylab(gettext("Density")) +
    ggplot2::xlab(gettext("X"))
  return(plot)
}

plotPdf.jaspDiscreteDistribution <- function(distribution, xRange, highlightDensity = NULL, highlightProbability = NULL) {
  xRange <- as.integer(xRange)
  yRange <- c(0, 0)
  plot <- ggplot2::ggplot()

  x <- xRange[1]:xRange[2]
  df <- data.frame(x = x, y = pdf(distribution, x))
  yRange <- range(c(yRange, df$y))

  if (nrow(df) <= 200) {
    plot <- plot +
      ggplot2::geom_segment(data = df, mapping = ggplot2::aes(x = x, xend = x, y = 0, yend = y))
  } else {
    plot <- plot +
      plotCurve(df)
  }

  if (nrow(df) <= 50) {
    plot <- plot +
      jaspGraphs::geom_point(data = df, mapping = ggplot2::aes(x = x, y = y))
  }

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(xRange)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(yRange)
  plot <- plot +
    jaspGraphs::themeJaspRaw() +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::scale_x_continuous(limits = range(xBreaks), breaks = xBreaks) +
    jaspGraphs::scale_y_continuous(limits = range(yBreaks), breaks = yBreaks) +
    ggplot2::ylab(gettext("Probability Mass")) +
    ggplot2::xlab(gettext("X"))

  return(plot)
}

plotCdf <- function(distribution, xRange, highlightDensity = FALSE, highlightProbability = FALSE) {
  UseMethod("plotCdf")
}

plotCdf.jaspContinuousDistribution <- function(distribution, xRange, highlightDensity = NULL, highlightProbability = NULL) {
  plot <- ggplot2::ggplot()
  yRange <- c(0, 1)
  x  <- seq(xRange[1], xRange[2], length.out = 101)
  df <- data.frame(x = x, y = cdf(distribution, x))
  plot <- plot + plotCurve(data = df)

  if (!is.null(highlightProbability)) {

  }

  if (!is.null(highlightDensity)) {
    x <- highlightDensity[highlightDensity >= xRange[1] & highlightDensity <= xRange[2]]
    y <- cdf(distribution, x)
    slope <- pdf(distribution, x)
    intercept <- y - slope * x
    label <- prettyFormat(slope) |> as.factor()

    df <- data.frame(x = x, y = y, slope = slope, intercept = intercept, label = label)

    plot <- plot +
      ggplot2::geom_abline(data = df, mapping = ggplot2::aes(intercept = intercept, slope = slope, col = label)) +
      jaspGraphs::geom_point(data = df, mapping = ggplot2::aes(x = x, y = y, col = label)) +
      ggplot2::scale_color_discrete(name = gettext("PDF"))
  }

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(xRange)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(yRange)
  plot <- plot +
    jaspGraphs::themeJaspRaw(legend.position = "right") +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::scale_x_continuous(limits = range(xBreaks), breaks = xBreaks) +
    jaspGraphs::scale_y_continuous(limits = range(yBreaks), breaks = yBreaks) +
    ggplot2::ylab(gettext("Probability")) +
    ggplot2::xlab(gettext("X"))

  return(plot)
}

plotCdf.jaspDiscreteDistribution <- function(distribution, xRange, highlightDensity = NULL, highlightProbability = NULL) {
  plot <- ggplot2::ggplot()
  xRange <- as.integer(xRange)
  yRange <- c(0, 1)
  x  <- xRange[1]:xRange[2]
  df <- data.frame(x = x, y = cdf(distribution, x))

  if (nrow(df) <= 200) {
    plot <- plot +
      ggplot2::geom_segment(data = df, mapping = ggplot2::aes(x = x, xend = x, y = 0, yend = y))
  } else {
    plot <- plot +
      plotCurve(df, shade = TRUE, shadeColor = adjustcolor("gray", alpha = 0.2))
  }

  if (nrow(df) <= 50) {
    plot <- plot + jaspGraphs::geom_point(data = df, mapping = ggplot2::aes(x = x, y = y))
  }

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(xRange)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(yRange)
  plot <- plot +
    jaspGraphs::themeJaspRaw(legend.position = "right") +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::scale_x_continuous(limits = range(xBreaks), breaks = xBreaks) +
    jaspGraphs::scale_y_continuous(limits = range(yBreaks), breaks = yBreaks) +
    ggplot2::ylab(gettext("Probability")) +
    ggplot2::xlab(gettext("X"))

  return(plot)
}


plotHist <- function(distribution, ...) {
  UseMethod("plotHist")
}

plotHist.jaspContinuousDistribution <- function(distribution, x, xName) {
  if (missing(xName))
    xName <- deparse1(substitute(x))

  plot <- jaspGraphs::jaspHistogram(x, xName, density = TRUE, rugs = TRUE, densityLineWidth = 0)
  dt <- ggplot2::layer_data(plot)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(x, dt$xmin, dt$xmax))
  xLimits <- range(xBreaks)
  x <- seq(xLimits[1], xLimits[2], length.out = 101)
  df <- data.frame(x = x, y = pdf(distribution, x))


  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(dt$y, df$y))
  yLimits <- range(yBreaks)
  plot <- plot + plotCurve(df) +
    ggplot2::scale_x_continuous(name = xName, breaks = xBreaks, limits = xLimits) +
    ggplot2::scale_y_continuous(name = gettext("Density"), breaks = yBreaks, limits = yLimits)

  return(plot)
}

plotEcdf <- function(distribution, ...) {
  UseMethod("plotEcdf")
}

plotEcdf.jaspContinuousDistribution <- function(distribution, x, xName) {
  if (missing(xName))
    xName <- deparse1(substitute(x))

  plot <- ggplot2::ggplot() +
    ggplot2::stat_ecdf(data = data.frame(x = x), mapping = ggplot2::aes(x = x), linewidth = 0.75) +
    ggplot2::geom_rug(data = data.frame(x = x), mapping = ggplot2::aes(x = x))

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(x)
  xLimits <- range(xBreaks)
  x <- seq(xLimits[1], xLimits[2], length.out=101)
  df <- data.frame(x = x, y = cdf(distribution, x))

  plot <- plot +
    plotCurve(df)

  plot <- plot +
    jaspGraphs::themeJaspRaw() +
    jaspGraphs::geom_rangeframe() +
    ggplot2::scale_x_continuous(name = xName, breaks = xBreaks, limits = xLimits) +
    ggplot2::scale_y_continuous(name = gettext("Probability"), limits = c(0, 1))

  return(plot)
}

plotQq <- function(distribution, ...) {
  UseMethod("plotQq")
}

plotQq.jaspContinuousDistribution <- function(distribution, x, xName, ci = TRUE, ciLevel = 0.95) {
  if (missing(xName))
    xName <- deparse1(substitute(x))

  x <- na.omit(x)
  sample <- sort(x)
  n      <- length(sample)
  p      <- stats::ppoints(n)


  theoretical <- qf(distribution, p)
  lmFit       <- lm(sample~theoretical)
  intercept   <- coefficients(lmFit)[[1]]
  slope       <- coefficients(lmFit)[[2]]

  df <- data.frame(sample = sample, theoretical = theoretical)

  if(isTRUE(ci)) {
    # Fox, J. (2016) Applied Regression Analysis and Generalized Linear Models, Third Edition. Sage.
    # Chapter 3.1.3
    alpha       <- 1-ciLevel
    pdf         <- pdf(distribution, theoretical)
    se <- sqrt(p * (1 - p) / n) * slope / pdf

    df[["upper"]] <- theoretical + se * qnorm(alpha/2, lower.tail = FALSE)
    df[["lower"]] <- theoretical + se * qnorm(alpha/2, lower.tail = TRUE)
    ciLayer <- list()
    ciLayer[[1]] <-
      ggplot2::geom_ribbon(
        mapping = ggplot2::aes(x = theoretical, ymin = lower, ymax = upper),
        fill = "steelblue", color = "black", alpha = 0.5
      )

    ciLayer[[2]] <- ggplot2::geom_line(mapping = ggplot2::aes(x = theoretical, y = theoretical), size = 1)

  } else {
    ciLayer <- NULL
  }

  yPoints <- as.vector(as.matrix(df))
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(yPoints)
  yLabs   <- jaspGraphs::axesLabeller(yBreaks)
  yRange  <- range(c(yPoints, yBreaks))
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(theoretical)
  xLabs   <- jaspGraphs::axesLabeller(xBreaks)
  xRange  <- range(c(theoretical, xBreaks))

  plot <- ggplot2::ggplot(data = df, ggplot2::aes(sample = sample)) +
    ciLayer +
    jaspGraphs::geom_abline2(mapping = ggplot2::aes(intercept = intercept, slope = slope), size = 1) +
    ggplot2::stat_qq(distribution = qf, dparams = list(distribution=distribution), shape = 21, fill = "grey", size = 3) +
    ggplot2::scale_y_continuous(name = gettext("Sample"),      breaks = yBreaks, labels = yLabs, limits = yRange) +
    ggplot2::scale_x_continuous(name = gettext("Theoretical"), breaks = xBreaks, labels = xLabs, limits = xRange) +
    jaspGraphs::themeJaspRaw() +
    jaspGraphs::geom_rangeframe()

  return(plot)
}

plotPp <- function(distribution, ...) {
  UseMethod("plotPp")
}

plotPp.jaspContinuousDistribution <- function(distribution, x, xName, ci = TRUE, ciLevel = 0.95) {
  if (missing(xName))
    xName <- deparse1(substitute(x))

  x <- na.omit(x)
  x <- sort(x)
  n <- length(x)
  sample <- stats::ppoints(n)
  theoretical <- cdf(distribution, x) |> sort()

  lmFit       <- lm(sample~theoretical)
  intercept   <- coefficients(lmFit)[[1]]
  slope       <- coefficients(lmFit)[[2]]

  df <- data.frame(sample = sample, theoretical = theoretical)

  if(isTRUE(ci)) {
    # Stirling, W. D. (1982). Enhancements to aid interpretation of probability plots. Journal of the Royal Statistical Society: Series D (The Statistician), 31(3), 211-220.
    # Quesenberry, C. P., & Hales, C. (1980). Concentration bands for uniformity plots. Journal of Statistical Computation and Simulation, 11(1), 41-53.
    i     <- seq_along(x)
    alpha <- 1-ciLevel

    df[["lower"]] <- qbeta(  alpha/2, i, n-i+1)
    df[["upper"]] <- qbeta(1-alpha/2, i, n-i+1)

    ciLayer <- list()
    ciLayer[[1]] <- ggplot2::geom_ribbon(
      mapping = ggplot2::aes(y = sample, xmin = lower, xmax = upper),
      fill = "steelblue", color = "black", alpha = 0.5
    )
    ciLayer[[2]] <- jaspGraphs::geom_abline2(slope = 1, intercept = 0, size = 1)
  } else {
    ciLayer <- NULL
  }

  plot <- ggplot2::ggplot(data = df) +
    ciLayer +
    jaspGraphs::geom_abline2(ggplot2::aes(intercept = intercept, slope = slope), size = 1) +
    jaspGraphs::geom_point(ggplot2::aes(x = theoretical, y = sample)) +
    ggplot2::xlab(gettext("Theoretical")) + ggplot2::ylab(gettext("Sample")) +
    ggplot2::scale_x_continuous(limits = 0:1, expand = ggplot2::expansion(mult = 0, add = c(0.05, 0.1))) +
    ggplot2::scale_y_continuous(limits = 0:1, expand = ggplot2::expansion(mult = 0, add = c(0.05, 0.1))) +
    jaspGraphs::themeJaspRaw() +
    jaspGraphs::geom_rangeframe()

  return(plot)

}
