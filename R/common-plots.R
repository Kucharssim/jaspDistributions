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

#' @export
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
      plot <- plot + plotCurve(data = df, line = FALSE, shade = TRUE)
    }
  }

  x  <- seq(xRange[1], xRange[2], length.out = 101)
  df <- data.frame(x = x, y = pdf(distribution, x))
  yRange <- range(c(yRange, df$y))
  plot <- plot + plotCurve(data = df)

  if (!is.null(highlightDensity)) {
    x <- highlightDensity[highlightDensity > xRange[1] & highlightDensity < xRange[2]]
    df <- data.frame(x = x, y = pdf(distribution, x))
    plot <- plot +
      jaspGraphs::geom_point(data = df, mapping = ggplot2::aes(x = x, y = y))
  }

  plot <- plot +
    jaspGraphs::themeJaspRaw() +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::scale_x_continuous(limits = xRange) +
    jaspGraphs::scale_y_continuous(limits = yRange) +
    ggplot2::ylab(gettext("Density")) +
    ggplot2::xlab(gettext("X"))
  return(plot)
}


