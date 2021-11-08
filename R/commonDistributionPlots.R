distributionR6$set(
  which = "public", name = "plotPDF",
  value = function(xRange, highlightProbability = FALSE, highlightDensity = FALSE, highlightLimits) {
    plot <- ggplot2::ggplot(data = data.frame(x = xRange), ggplot2::aes(x = x)) +
      ggplot2::stat_function(fun = self$pdf, n = 101, size = 1.25)

    if(highlightProbability) {
      plot <- private$highlightProbability(plot, xRange, highlightLimits)
    }

    if(highlightDensity) {
      plot <- private$highlightDensity(plot, xRange, highlightLimits)
    }

    lineData <- ggplot2::layer_data(plot, 1)
    lineData <- lineData[is.finite(lineData$y),]
    plot <- plot + ggplot2::ylab(gettext("Density")) +
      ggplot2::scale_x_continuous(limits = xRange,  breaks = jaspGraphs::getPrettyAxisBreaks(xRange)) +
      ggplot2::scale_y_continuous(limits = c(0, max(lineData$y)), breaks = jaspGraphs::getPrettyAxisBreaks(lineData$y), expand = ggplot2::expansion(mult=c(0,0.2)))


    plot <- jaspGraphs::themeJasp(plot)
    return(plot)
  },
  overwrite = TRUE
)

distributionR6$set(
  which = "private", name = "highlightProbability",
  value = function(plot, xRange, highlightLimits) {
    # determine plotting region
    args <- options[['pars']]
    argsPDF <- options[['pars']]

    # calculate value under the curve
    cdfValue <- self$cdf(highlightLimits)
    cdfValue <- cdfValue[2] - cdfValue[1]

    # round value under the curve for plotting
    cdfValueRound <- round(cdfValue, 2)
    for(i in seq_along(cdfValueRound)) {
      if(cdfValueRound[i] %in% c(0, 1)) {
        cdfValueRound[i] <- round(cdfValue, 3)
      }
    }

    newHighlightLimits <- highlightLimits
    if(is.infinite(newHighlightLimits[1])) newHighlightLimits[1] <- xRange[1]
    if(is.infinite(newHighlightLimits[2])) newHighlightLimits[2] <- xRange[2]

    # calculate position of the geom_text
    x <- seq(newHighlightLimits[1], newHighlightLimits[2], length.out = 20)
    w <- self$pdf(x)
    x <- x[!is.na(w)]
    w <- w[!is.na(w)]
    x <- weighted.mean(x, w)
    y <- self$pdf(x)

    xx <- seq(newHighlightLimits[1], newHighlightLimits[2], length.out = 101)
    max_y <- max(self$pdf(xx), na.rm = TRUE)

    if(y < 0.1*max_y) y <- y*5 else y <- y/3

    plot <- plot +
      ggplot2::stat_function(fun = self$pdf, n = 101, geom = "area",
                             xlim = newHighlightLimits, fill = "steelblue") +
      ggplot2::geom_text(data = data.frame(x = x, y = y, label = cdfValueRound),
                         mapping = ggplot2::aes(x = x, y = y, label = label), size = 8, parse = TRUE)

    return(plot)
  },
  overwrite = TRUE
)

distributionR6$set(
  which = "private", name = "highlightDensity",
  value = function(plot, xRange, highlightLimits) {
    pdfValue <- self$pdf(highlightLimits)

    segment_data <- data.frame(x = xRange[1] + (xRange[2]-xRange[1])/15,
                               xend = highlightLimits, y = pdfValue)

    # plot density
    plot <- plot +
      ggplot2::geom_segment(data = segment_data,
                            mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = y),
                            linetype = 2) +
      ggplot2::geom_text(data = data.frame(x = xRange[1], y = pdfValue, label = round(pdfValue, 2)),
                         ggplot2::aes(x = x, y = y, label = label), size = 6) +
      ggplot2::geom_linerange(x = highlightLimits, ymin = 0, ymax = pdfValue, linetype = 2) +
      jaspGraphs::geom_point(x = highlightLimits, y = pdfValue, size = 5)

    return(plot)
  },
  overwrite = TRUE
)
