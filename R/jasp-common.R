jaspCommonDistribution <- function(distribution, jaspResults, dataset, options) {
  UseMethod("jaspCommonDistribution")
}

jaspCommonDistribution.jaspContinuousDistribution <- function(distribution, jaspResults, dataset, options) {
  jaspParametersSupportMoments(distribution, jaspResults, options)
  for(what in c("pdf", "cdf", "qf")) {
    jaspTheoreticalPlot(distribution, jaspResults, options, what = what)
  }
  jaspDistributionTable(distribution, jaspResults, options)
  data <- jaspGenerateData(distribution, jaspResults, dataset, options)
  jaspDisplayData(distribution, jaspResults, data, options)
}

jaspParametersSupportMoments <- function(distribution, jaspResults, options) {
  if(!isTRUE(options[["parsSupportMoments"]])) return()
  if(!is.null(jaspResults[["parsSupportMoments"]])) return()

  jaspResults[["parsSupportMoments"]] <- jaspBase::createJaspContainer(
    title = gettext("Parameters, Support, and Moments"),
    dependencies = c("parsSupportMoments", jaspDependencies(distribution))
    )

  distSummary <- summary(distribution, mode = "u")
  pars <- distSummary[["parameters"]][["parameters"]][, c("name", "latex", "support", "value")]
  jaspResults[["parsSupportMoments"]][["pars"]] <- jaspBase::createJaspTable(
    title      = gettext("Parameters"),
    colNames   = c("name", "latex", "support", "value"),
    colTitles  = c("",     gettext("Symbol"), gettext("Support"), gettext("Value")),
    colFormats = c("string", "string", "string", "number"),
    data       = pars
  )

  jaspResults[["parsSupportMoments"]][["support"]] <- jaspBase::createJaspHtml(
    title = gettext("Support"), text = distSummary[["support"]]
  )

  jaspResults[["parsSupportMoments"]][["moments"]] <- jaspBase::createJaspTable(
    title = gettext("Moments"),
    colTitles = c(gettext("Moment"), gettext("Symbol"), gettext("Expression"), gettext("Value")),
    data = distSummary[["moments"]]
  )
}

jaspDistributionTable <- function(distribution, jaspResults, options) {
  UseMethod("jaspDistributionTable")
}

jaspDistributionTable.jaspContinuousDistribution <- function(distribution, jaspResults, options) {
  x <- seq(-3, 3, by = 0.5)
  df <- data.frame(x = x, pdf = pdf(distribution, x), cdf = cdf(distribution, x))
  jaspResults[["table"]] <- jaspBase::createJaspTable(
    title = gettext("Distribution table"),
    data = df,
    colTitles = c("X", gettext("PDF"), gettext("CDF")))
}

jaspTheoreticalPlot <- function(distribution, jaspResults, options, what = c("pdf", "cdf", "qf")) {
  which <- match.arg(what)
  title <- switch(
    what,
    pdf = gettext("Probability density function"),
    cdf = gettext("Cumulative distribution function"),
    qf  = gettext("Quantile function")
  )
  plotName <- switch(
    what,
    pdf = gettext("Density plot"),
    cdf = gettext("Cumulative probability plot"),
    qf  = gettext("Quantile plot")
  )
  dependencies <- switch(what, pdf = "plotPDF", cdf = "plotCDF", qf = "plotQF")
  dependencies <- c(dependencies, jaspDependencies(distribution))

  jaspResults[[what]] <- jaspBase::createJaspContainer(title = title, dependencies = dependencies)
  jaspResults[[what]][["plot"]] <- jaspBase::createJaspPlot(title = plotName)

  plot <- try(plot(distribution, what = what, xRange = c(-3, 3)))

  if(jaspBase::isTryError(plot)) {
    jaspResults[[what]][["plot"]]$plotObject <- ggplot2::ggplot()
    jaspResults[[what]][["plot"]]$setError(jaspBase::.extractErrorMessage(plot))
  } else {
    jaspResults[[what]][["plot"]]$plotObject <- plot
  }
}

jaspGenerateData <- function(distribution, jaspResults, dataset, options) {
  UseMethod("jaspGenerateData")
}

jaspGenerateData.jaspContinuousDistribution <- function(distribution, jaspResults, dataset, options) {
  rng(distribution, 100)
}


jaspDisplayData <- function(distribution, jaspResults, data, options) {
  UseMethod("jaspDisplayData")
}

jaspDisplayData.jaspContinuousDistribution <- function(distribution, jaspResults, data, options) {
  dataContainer <- jaspBase::createJaspContainer(title = gettext("Data overview"))
  statContainer <- jaspBase::createJaspContainer(title = gettext("Statistics"))
  plotContainer <- jaspBase::createJaspContainer(title = gettext("Plots"))

  for(what in c("histogram", "ecdf", "qq", "pp")) {
    jaspEmpiricalPlot(distribution, plotContainer, data, options, what = what)
  }

  dataContainer[["stats"]] <- statContainer
  dataContainer[["plots"]] <- plotContainer
  jaspResults[["dataOverview"]] <- dataContainer
}

jaspEmpiricalPlot <- function(distribution, jaspResults, data, options, what = c("histogram", "ecdf", "qq", "pp")) {
  UseMethod("jaspEmpiricalPlot")
}

jaspEmpiricalPlot.jaspContinuousDistribution <- function(distribution, jaspResults, data, options, what = c("histogram", "ecdf", "qq", "pp")) {
  what <- match.arg(what)
  title <- switch(
    what,
    histogram = gettext("Histogram vs. Theoretical PDF"),
    ecdf      = gettext("Empirical vs. Theoretical CDF"),
    qq        = gettext("Q-Q Plot"),
    pp        = gettext("P-P Plot")
  )
  jaspResults[[what]] <- jaspBase::createJaspPlot(title = title)
  plot <- try(plot.jaspDistribution(distribution, what = what, x = data))

  if(jaspBase::isTryError(plot)) {
    jaspResults[[what]]$plotObject <- ggplot2::ggplot()
    jaspResults[[what]]$setError(jaspBase::.extractErrorMessage(plot))
  } else {
    jaspResults[[what]]$plotObject <- plot
  }
}
