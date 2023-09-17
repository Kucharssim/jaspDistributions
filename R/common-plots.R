#' @export
plot.jaspDistribution <- function(distribution, what = c("pdf", "cdf", "qf"), ...) {
  what <- match.arg(what)
  switch(what,
         pdf = plotPDF(distribution, ...),
         cdf = plotCDF(distribution, ...),
         qf  = plotCDF(distribution, ...) + ggplot2::coord_flip())
}

# plotting functions
plotCurve <- function(data, line = TRUE, lineColor = "black", lineSize = 1.25, shade  = FALSE, shadeColor = "steelblue", shadeAlpha = 1) {
  out <- list()
  if(shade)
    out[[1]] <- ggplot2::geom_ribbon(data = data, mapping = ggplot2::aes(x = x, ymin = 0, ymax = y), inherit.aes = FALSE, fill = shadeColor, alpha = shadeAlpha)
  if(line)
    out[[2]] <- ggplot2::geom_line(data = data, mapping = ggplot2::aes(x = x, y = y), inherit.aes = FALSE, size = lineSize, color = lineColor)

  return(out)
}

plotPDF <- function(distribution, xRange, highlightDensity = FALSE, highlightProbability = FALSE, highlightRange = NULL) {
  UseMethod("plotPDF")
}

plotPDF.jaspContinuousDistribution <- function(distribution, xRange, highlightDensity = NULL, highlightProbability = NULL) {
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

plotPDF.jaspDiscreteDistribution <- function(distribution, xRange, highlightDensity = NULL, highlightProbability = NULL) {
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

plotCDF <- function(distribution, xRange, highlightDensity = FALSE, highlightProbability = FALSE, highlightRange = NULL) {
  UseMethod("plotCDF")
}

plotCDF.jaspContinuousDistribution <- function(distribution, xRange, highlightDensity = NULL, highlightProbability = NULL) {
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

plotCDF.jaspDiscreteDistribution <- function(distribution, xRange, highlightDensity = NULL, highlightProbability = NULL) {
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
