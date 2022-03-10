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
plotPDF.jaspContinuousDistribution <- function(distribution, xRange, highlightDensity = FALSE, highlightProbability = FALSE, highlightRange = NULL) {
  layers <- list()
  if(highlightProbability && !is.null(highlightRange)) {
    x  <- seq(highlightRange[1], highlightRange[2], length.out = 101)
    df <- data.frame(x = x, y = pdf(distribution, x))

    layers[[1]] <- plotCurve(data = df, line = FALSE, shade = TRUE)
  }

  if(highlightDensity) {

  }

  x  <- seq(xRange[1], xRange[2], length.out = 101)
  df <- data.frame(x = x, y = pdf(distribution, x))
  layers[[3]] <- plotCurve(data = df)

  return(combineLayers(layers))
}

combineLayers <- function(layers) {
  plot <- ggplot2::ggplot() +
    layers

  return(plot)
}
