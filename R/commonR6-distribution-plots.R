distributionR6$set(
  which = "public", name = "plotPDF",
  value = function(xRange, highlightProbability = FALSE, highlightDensity = FALSE, highlightInterval) {
    plot <- ggplot2::ggplot(data = data.frame(x = xRange), ggplot2::aes(x = x)) +
      ggplot2::stat_function(fun = self$pdf, n = 101, size = 1.25)

    if(highlightProbability) {
      # calculate value under the curve
      cdfValue <- self$cdf(highlightInterval)
      cdfValue <- cdfValue[2] - cdfValue[1]

      # round value under the curve for plotting
      cdfValueRound <- round(cdfValue, 2)
      for(i in seq_along(cdfValueRound)) {
        if(cdfValueRound[i] %in% c(0, 1)) {
          cdfValueRound[i] <- round(cdfValue, 3)
        }
      }

      newhighlightInterval <- highlightInterval
      if(is.infinite(newhighlightInterval[1])) newhighlightInterval[1] <- xRange[1]
      if(is.infinite(newhighlightInterval[2])) newhighlightInterval[2] <- xRange[2]

      # calculate position of the geom_text
      x <- seq(newhighlightInterval[1], newhighlightInterval[2], length.out = 20)
      w <- self$pdf(x)
      x <- x[!is.na(w)]
      w <- w[!is.na(w)]
      x <- weighted.mean(x, w)
      y <- self$pdf(x)

      xx <- seq(newhighlightInterval[1], newhighlightInterval[2], length.out = 101)
      max_y <- max(self$pdf(xx), na.rm = TRUE)

      if(y < 0.1*max_y) y <- y*5 else y <- y/3

      plot <- plot +
        ggplot2::stat_function(fun = self$pdf, n = 101, geom = "area",
                               xlim = newhighlightInterval, fill = "steelblue") +
        ggplot2::geom_text(data = data.frame(x = x, y = y, label = cdfValueRound),
                           mapping = ggplot2::aes(x = x, y = y, label = label), size = 8, parse = TRUE)
    }

    if(highlightDensity) {
      pdfValue <- self$pdf(highlightInterval)

      segment_data <- data.frame(x = xRange[1] + (xRange[2]-xRange[1])/15,
                                 xend = highlightInterval, y = pdfValue)

      # plot density
      plot <- plot +
        ggplot2::geom_segment(data = segment_data,
                              mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = y),
                              linetype = 2) +
        ggplot2::geom_text(data = data.frame(x = xRange[1], y = pdfValue, label = round(pdfValue, 2)),
                           ggplot2::aes(x = x, y = y, label = label), size = 6) +
        ggplot2::geom_linerange(x = highlightInterval, ymin = 0, ymax = pdfValue, linetype = 2) +
        jaspGraphs::geom_point(x = highlightInterval, y = pdfValue, size = 5)
    }

    lineData <- ggplot2::layer_data(plot, 1)
    lineData <- lineData[is.finite(lineData$y),]
    plot <- plot + ggplot2::ylab(gettext("Density")) +
      ggplot2::scale_x_continuous(limits = xRange,  breaks = jaspGraphs::getPrettyAxisBreaks(xRange)) +
      ggplot2::scale_y_continuous(limits = c(0, max(lineData$y)), breaks = jaspGraphs::getPrettyAxisBreaks(lineData$y), expand = ggplot2::expansion(mult=c(0,0.2)))


    plot <- jaspGraphs::themeJasp(plot)
    return(plot)
  }
)


distributionR6$set(
  which = "public", name = "plotHistogram",
  value = function(variableName, histogramBins = NULL) {
    variable <- self$getData()
    range <- range(variable)
    histData <- hist(variable,
                     breaks = seq(range[1], range[2], length.out = histogramBins+1),
                     plot = FALSE)
    dat <- data.frame(counts = histData$counts, density = histData$density, mids = histData$mids)

    plot <- ggplot2::ggplot(data = dat, ggplot2::aes(x = mids, y = counts/sum(counts))) +
      ggplot2::geom_bar(stat="identity", fill = "grey", colour = "black") +
      ggplot2::xlab(variableName) +
      ggplot2::ylab(gettextf("Rel. Freq (%s in bin)", variableName))

    plot <- plot + ggplot2::scale_x_continuous(limits = range,
                                               expand = c(0.05, 0),
                                               breaks = jaspGraphs::getPrettyAxisBreaks(range))

    plot <- jaspGraphs::themeJasp(plot)
    return(plot)
  }
)

distributionR6$set(
  which = "public", name = "plotECDF",
  value = function(variableName) {
    variable <- self$getData()
    p <- ggplot2::ggplot(data = data.frame(variable = variable), ggplot2::aes(x = variable)) +
      ggplot2::stat_ecdf(geom = "step", size = 1.5) +
      ggplot2::geom_rug() +
      ggplot2::scale_x_continuous(limits = range(variable)*1.1) +
      ggplot2::xlab(variableName) +
      ggplot2::ylab(substitute(f~(v <= x), list(f = gettext("Freq"), v = variableName)))

    p <- jaspGraphs::themeJasp(p)
  }
)
