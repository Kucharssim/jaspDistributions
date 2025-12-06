# Main function ----
commonResults <- S7::new_generic("commonResults", "distribution")

S7::method(commonResults, DistributionS7::DistributionContinuous) <- function(
    distribution, jaspResults, dataset, options, state=NULL
) {

  theoreticalDistribution(distribution, jaspResults, options)

  # sim/retrieve data
  variable <- generateAndGetData(distribution, jaspResults, dataset, options)

  dataDescriptives(distribution, jaspResults, variable, options)

  # from now on the distribution object is fitted to data (if fitting requested)
  distribution <- fitDistribution(distribution, jaspResults, variable, options)

  parameterTable(distribution, jaspResults, variable, options)

  fitAssessment(distribution, jaspResults, variable, options)

}

# Theoretical distribution ----
theoreticalDistribution <- S7::new_generic("theoreticalDistribution", "distribution")

S7::method(theoreticalDistribution, DistributionS7::DistributionContinuous) <- function(
    distribution, jaspResults, options
) {

  container <- jaspResults[["theoreticalDistribution"]] %setOrRetrieve%
    createJaspContainer(
      title = gettext("Theoretical distribution"),
      dependencies = dependencies(distribution)
    )

  pdfPlot(distribution, container, options)
  cdfPlot(distribution, container, options)
  qfPlot (distribution, container, options)

  distributionTable(distribution, container, options)
}
## Density/MASS ----
pdfPlot <- S7::new_generic("pdfPlot", "distribution")

S7::method(pdfPlot, DistributionS7::DistributionContinuous) <- function(
    distribution, jaspResults, options
) {
  if (!options[["pdfPlot"]]) return()

  container <- createJaspContainer(title = gettext("Probability Density Function"),
                                   dependencies = c(dependencies(distribution), "pdfPlot", "explanatoryText")
                                   )
  jaspResults[["pdfPlot"]] <- container

  if (options[["explanatoryText"]])
    container[["explanatoryText"]] <- createJaspHtml(text = .ldAllTextsList()$explanations$pdf, position = 1)

  container[["plot"]] <- createJaspPlot(title=gettext("Density plot"), position = 2)

  plot <- DistributionS7::plot_pdf(distribution)

  container[["plot"]]$plotObject <- plot
}

## CDF ----
cdfPlot <- S7::new_generic("cdfPlot", "distribution")

S7::method(cdfPlot, DistributionS7::DistributionContinuous) <- function(
    distribution, jaspResults, options
) {
  if (!options[["cdfPlot"]]) return()

  container <- createJaspContainer(title = gettext("Cumulative Distribution Function"),
                                   dependencies = c(dependencies(distribution), "cdfPlot", "explanatoryText")
  )
  jaspResults[["cdfPlot"]] <- container

  if (options[["explanatoryText"]])
    container[["explanatoryText"]] <- createJaspHtml(text = .ldAllTextsList()$explanations$cdf, position = 1)

  container[["plot"]] <- createJaspPlot(title=gettext("Cumulative probability plot"), position = 2)

  plot <- DistributionS7::plot_cdf(distribution)

  container[["plot"]]$plotObject <- plot
}

## Quantile ----
qfPlot <- S7::new_generic("qfPlot", "distribution")

S7::method(qfPlot, DistributionS7::DistributionContinuous) <- function(
    distribution, jaspResults, options
) {
  if (!options[["qfPlot"]]) return()

  container <- createJaspContainer(title = gettext("Quantile Function"),
                                   dependencies = c(dependencies(distribution), "qfPlot", "explanatoryText")
  )
  jaspResults[["qfPlot"]] <- container

  if (options[["explanatoryText"]])
    container[["explanatoryText"]] <- createJaspHtml(text = .ldAllTextsList()$explanations$qf, position = 1)

  container[["plot"]] <- createJaspPlot(title=gettext("Quantile plot"), position = 2)

  plot <- DistributionS7::plot_qf(distribution)

  container[["plot"]]$plotObject <- plot
}

## Distribution table -----
distributionTable <- S7::new_generic("distributionTable", "distribution")

S7::method(distributionTable, DistributionS7::DistributionContinuous) <- function(
    distribution, jaspResults, options
) {
  if (!options[["distributionTable"]]) return()

  table <- createJaspTable(
    title = gettext("Distribution table"),
    dependencies = c(dependencies(distribution), "distributionTable"),
    expectedRows = options[["distributionTableLength"]]
  )
  jaspResults[["distributionTable"]] <- table

  table$addColumnInfo(name = "x",    title = gettext("Quantile"),       type = "number")
  table$addColumnInfo(name = "pdf",  title = gettext("Density"),        type = "number")
  table$addColumnInfo(name = "cdf",  title = gettext("Less than x"),    type = "number", overtitle = gettext("Cumulative probability"))
  table$addColumnInfo(name = "ccdf", title = gettext("Greater than x"), type = "number", overtitle = gettext("Cumulative probability"))

  if (options[["distributionTableLength"]] < 1L) return()

  x <- seq(options[["distributionTableMin"]], options[["distributionTableMax"]], length.out = options[["distributionTableLength"]])

  if (options[["distributionTableRangeBasedOn"]] == "cdf") x <- DistributionS7::qf(distribution, x)

  cumulativeProbs <- DistributionS7::cdf(distribution, x)

  df <- data.frame(x = x, pdf = DistributionS7::pdf(distribution, x), cdf = cumulativeProbs, ccdf=1-cumulativeProbs)

  table$setData(df)
}

# Data -----
## Generate and get ----
generateAndGetData <- S7::new_generic("simulateAndGetData", "distribution")

S7::method(generateAndGetData, DistributionS7::DistributionContinuous) <- function(
    distribution, jaspResults, dataset, options
) {
  if (options[["generateData"]] && is.null(jaspResults[["generatedData"]])) {
    jaspResults[["generatedData"]] <- createJaspColumn(
      columnName = options[["generatedDataColumn"]],
      dependencies = c(dependencies(distribution), "generateData", "generatedDataColumn")
      )
    jaspResults[["generatedData"]]$setScale(DistributionS7::rng(distribution, nrow(dataset)))
  }

  variable <- NULL

  if (options[["variable"]] != "") {
    variable <- dataset[[options[["variable"]]]]
  }

  return(variable)
}

## describe data ----
dataDescriptives <- S7::new_generic("dataDescriptives", "distribution")

S7::method(dataDescriptives, DistributionS7::DistributionContinuous) <- function(
    distribution, jaspResults, variable, options
) {
  if (!options[["dataDescriptives"]] && !options[["dataHistogram"]]) return()

  container <- jaspResults[["dataContainer"]] %setOrRetrieve% createJaspContainer(
    title = gettext("Data overview"),
    dependencies = c(dependencies(distribution), "variable", "generateData")
  )

  if (options[["dataDescriptives"]] && is.null(container[["dataDescriptives"]])) {
    table <- createJaspTable(
      title = gettext("Descriptives"),
      dependencies = "dataDescriptives"
      )

    table$addColumnInfo(name = "variable",   title = gettext("Variable"),       type = "string")
    table$addColumnInfo(name = "sampleSize", title = gettext("n"),              type = "integer")
    table$addColumnInfo(name = "mean",       title = gettext("Mean"),           type = "number")
    table$addColumnInfo(name = "var",        title = gettext("Variance"),       type = "number")
    table$addColumnInfo(name = "sd",         title = gettext("Std. deviation"), type = "number")
    table$addColumnInfo(name = "min",        title = gettext("Minimum"),        type = "number")
    table$addColumnInfo(name = "quantile25", title = gettextf("25%% Quantile"), type = "number")
    table$addColumnInfo(name = "median",     title = gettext("Median"),         type = "number")
    table$addColumnInfo(name = "quantile75", title = gettextf("75%% Quantile"), type = "number")
    table$addColumnInfo(name = "max",        title = gettext("Maximum"),        type = "number")

    container[["dataDescriptives"]] <- table

    table$setData(
      list(
        variable   = options[["variable"]],
        sampleSize = length(variable),
        mean       = mean(variable),
        var        = var(variable),
        sd         = sd(variable),
        min        = min(variable),
        quantile25 = quantile(variable, .25),
        median     = median(variable),
        quantile75 = quantile(variable, .75),
        max        = max(variable)
      )
    )
  }

  if (options[["dataHistogram"]] && is.null(container[["dataHistogram"]])) {
    plot <- createJaspPlot(
      plot = gettext("Histogram"),
      dependencies = "dataHistogram"
    )

    container[["dataHistogram"]] <- plot

    plot$plotObject <- jaspGraphs::jaspHistogram(variable, xName = options[["variable"]], density=TRUE)
  }

}


# Fit distribution ----
fitDistribution <- S7::new_generic("fitDistribution", "distribution")

S7::method(fitDistribution, DistributionS7::Distribution) <- function(
    distribution, jaspResults, variable, options
) {
  if (!options[["estimateParameters"]] || is.null(variable)) return(distribution)
  # fix state
  distribution <- DistributionS7::fit_distribution(distribution, data=variable)
  return(distribution)
}

## Parameter table ----
parameterTable <- S7::new_generic("parameterTable", "distribution")

S7::method(parameterTable, DistributionS7::Distribution) <- function(
    distribution, jaspResults, variable, options
) {
  if (!options[["parameterTable"]]) return()
  if (!is.null(jaspResults[["parameterTable"]])) return()

  table <- createJaspTable(
    title = if (options[["estimateParameters"]]) gettext("Parameter estimates") else gettext("Parameters"),
    dependencies = c(dependencies(distribution), "variable", "generateData", "estimateParameters", "parameterTable", "parameterTableUncertainty", "parameterTableCiLevel")
  )
  table$showSpecifiedColumnsOnly <- TRUE
  jaspResults[["parameterTable"]] <- table

  table$addColumnInfo(name = "label",    title = gettext("Parameter"))
  table$addColumnInfo(name = "estimate", title = if (options[["estimateParameters"]]) gettext("Estimate") else gettext("Value"))

  if (options[["estimateParameters"]] && options[["parameterTableUncertainty"]]) {
    results <- DistributionS7::parameter_inference(distribution, data=variable, ci_level = options[["parameterTableCiLevel"]])

    if ("se" %in% colnames(results))
      table$addColumnInfo(name = "se", title = gettext("Std. Error"), type = "number")
    if ("lower" %in% colnames(results))
      table$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = gettextf("%1$s%% Confidence Interval", 100 * options[["parameterTableCiLevel"]]))
    if ("upper" %in% colnames(results))
      table$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = gettextf("%1$s%% Confidence Interval", 100 * options[["parameterTableCiLevel"]]))

    results[["label"]] <- unlist(DistributionS7::parameter_properties(distribution, property="label", which="free"))
  } else {
    which <- if (options[["estimateParameters"]]) "free" else "all"
    results <- list(
      label    = unlist(DistributionS7::parameter_properties(distribution, property="label", which=which)),
      estimate = unlist(DistributionS7::parameter_properties(distribution, property="value", which=which))
    )
  }

  results[["label"]] <- mathExpression(results[["label"]], inline = TRUE)


  table$setData(results)
}

# Fit assessment -----
fitAssessment <- S7::new_generic("fitAssessment", "distribution")

S7::method(fitAssessment, DistributionS7::Distribution) <- function(
    distribution, jaspResults, variable, options
) {
  if (is.null(variable)) return()

  container <- jaspResults[["fitAssessment"]] %setOrRetrieve%
    createJaspContainer(
      title = gettext("Fit assessment"),
      dependencies = c(dependencies(distribution), "variable", "generateData", "estimateParameters")
    )

  fitTables(distribution, container, variable, options)
  fitPlots(distribution, container, variable, options)
}

## Fit tables ----
fitTables <- S7::new_generic("fitTables", "distribution")

S7::method(fitTables, DistributionS7::Distribution) <- function(
    distribution, jaspResults, variable, options
) {
  goodnessOfFit(distribution, jaspResults, variable, options)
  informationCriteria(distribution, jaspResults, variable, options)
  momentsTable(distribution, jaspResults, variable, options)
}

### Goodness of fit ----
goodnessOfFit <- S7::new_generic("goodnessOfFit", "distribution")

S7::method(goodnessOfFit, DistributionS7::Distribution) <- function(
    distribution, jaspResults, variable, options
) {
  if (!options[["goodnessOfFit"]]) return()
  table <- createJaspTable(
    title = gettext("Goodness of fit tests"),
    dependencies = c("goodnessOfFit", "goodnessOfFitBootstrap", "goodnessOfFitBootstrapSamples")
  )
  jaspResults[["goodnessOfFit"]] <- table

  table$addColumnInfo(name = "test", title = gettext("Test"), type = "string")
  table$addColumnInfo(name = "statistic", title = gettext("Statistic"), type = "number")
  table$addColumnInfo(name = "p_value", title = gettext("p"), type="pvalue")

  if (options[["goodnessOfFitBootstrap"]])
    table$addFootnote(
      message = gettextf("Estimated using %1$i parametric bootstrap samples.", options[["goodnessOfFitBootstrapSamples"]]),
      colNames = "p_value")

  results <- try(
    DistributionS7::fit_statistics_absolute(
      distribution, variable,
      estimated = options[["estimateParameters"]],
      bootstrap = if (options[["goodnessOfFitBootstrap"]]) options[["goodnessOfFitBootstrapSamples"]] else 0L
      )
  )

  if (isTryError(results)) {
    table$setError(gettextf("Could not calculate goodness of fit with the following error: %1$s", .extractErrorMessage(results)))
    return()
  }

  results[["test"]] <- .goodnessOfFitTestLabels(results[["test"]])

  table$setData(results)
}

.goodnessOfFitTestLabels <- function(test) {
  labels <- list(
    ks_test = gettext("Kolmogorov–Smirnov"),
    lillie_test = gettext("Lilliefors"),
    cvm_test = gettext("Cramér–von Mises"),
    ad_test = gettext("Anderson–Darling"),
    shapiro_wilk_test = gettext("Shapiro-Wilk"),
    shapiro_francia_test = gettext("Shapiro-Francia")
  )

  return(labels[test])
}

### Information criteria ----
informationCriteria <- S7::new_generic("informationCriteria", "distribution")

S7::method(informationCriteria, DistributionS7::Distribution) <- function(
    distribution, jaspResults, variable, options
) {
  if (!options[["informationCriteria"]]) return()

  table <- createJaspTable(
    title = gettext("Information criteria"),
    dependencies = c("informationCriteria")
  )
  jaspResults[["informationCriteria"]] <- table

  table$addColumnInfo(name = "n_obs",   title = gettext("n"),         type="number")
  table$addColumnInfo(name = "n_par",   title = gettext("df"),        type="number")
  table$addColumnInfo(name = "log_lik", title = gettext("Log. lik."), type="number")
  table$addColumnInfo(name = "aic",     title = gettext("AIC"),       type="number")
  table$addColumnInfo(name = "bic",     title = gettext("BIC"),       type="number")

  results <- try(
    DistributionS7::fit_statistics_relative(
      distribution, variable#, estimated = options[["estimateParameters"]]
    )
  )

  if (isTryError(results)) {
    table$setError(gettextf("Could not calculate information criteria with the following error: %1$s", .extractErrorMessage(results)))
    return()
  }

  table$setData(results)
}

### Moments ----
momentsTable <- S7::new_generic("momentsTable", "distribution")

S7::method(momentsTable, DistributionS7::DistributionContinuous) <- function(
    distribution, jaspResults, variable, options
) {
  if (!options[["momentsTable"]]) return()

  table <- createJaspTable(
    title = "Moments",
    dependencies = c("momentsTable", "momentsTableNumber"),
    expectedRows = options[["momentsTableNumber"]]
  )

  table$addColumnInfo(name = "moment",          title = gettext("Moment"),      type = "integer")
  table$addColumnInfo(name = "raw_observed",    title = gettext("Observed"),    type = "number", overtitle = gettext("Raw"))
  table$addColumnInfo(name = "raw_theoretical", title = gettext("Theoretical"), type = "number", overtitle = gettext("Raw"))
  table$addColumnInfo(name = "cen_observed",    title = gettext("Observed"),    type = "number", overtitle = gettext("Central"))
  table$addColumnInfo(name = "cen_theoretical", title = gettext("Theoretical"), type = "number", overtitle = gettext("Central"))
  table$addColumnInfo(name = "std_observed",    title = gettext("Observed"),    type = "number", overtitle = gettext("Standardized"))
  table$addColumnInfo(name = "std_theoretical", title = gettext("Theoretical"), type = "number", overtitle = gettext("Standardized"))

  jaspResults[["momentsTable"]] <- table

  results <- matrix(nrow = options[["momentsTableNumber"]], ncol = 7)
  colnames(results) <- c("moment",
                         "raw_observed", "raw_theoretical",
                         "cen_observed", "cen_theoretical",
                         "std_observed", "std_theoretical")

  for (moment in seq_len(options[["momentsTableNumber"]])) {
    results[moment, "moment"] <- moment
    for (type in c("raw", "cen", "std")) {
      typeFull <- switch(type, raw="raw", cen="central", std="standardized")

      results[moment, paste(type, "observed",    sep="_")] <- .observedMoment(variable,  moment=moment, type=typeFull)
      results[moment, paste(type, "theoretical", sep="_")] <- DistributionS7::moment(distribution, moment=moment, type=typeFull)
    }
  }

  table$setData(results)
}

.observedMoment <- function(x, moment, type) {
  type <- match.arg(type, choices=c("raw", "central", "standardized"))
  stopifnot(moment > 0)

  if (moment == 1 && type %in% c("central", "standardized")) return(0)
  if (moment == 2 && type == "standardized") return(1)

  if (type == "raw") {
    result <- mean(x^moment)
  } else if (type == "central") {
    m <- mean(x)
    result <- mean((x-m)^moment)
  } else {
    m <- mean(x)
    sd <- sd(x)
    result <- mean(((x-m)/sd)^moment)
  }

  return(result)
}
## Fit plots ----
fitPlots <- S7::new_generic("fitPlots", "distribution")

S7::method(fitPlots, DistributionS7::Distribution) <- function(
    distribution, jaspResults, variable, options
) {
  container <- jaspResults[["fitPlots"]] %setOrRetrieve%
    createJaspContainer(title = gettext("Plots"))

  histPlot (distribution, container, variable, options)
  qqPlot   (distribution, container, variable, options)
  ecdfPlot (distribution, container, variable, options)
  ppPlot   (distribution, container, variable, options)
}

histPlot <- S7::new_generic("histPlot", "distribution")

S7::method(histPlot, DistributionS7::DistributionContinuous) <- function(
    distribution, jaspResults, variable, options
) {
  if (!options[["histPlot"]]) return()
  plot <- createJaspPlot(
    title = gettext("Histogram vs. Theoretical PDF"),
    dependencies = "histPlot"
  )
  jaspResults[["histPlot"]] <- plot

  plot$plotObject <- DistributionS7::plot_hist(distribution, variable, name = options[["variable"]])
}

qqPlot <- S7::new_generic("qqPlot", "distribution")

S7::method(qqPlot, DistributionS7::DistributionContinuous) <- function(
    distribution, jaspResults, variable, options
) {
  if (!options[["qqPlot"]]) return()
  plot <- createJaspPlot(
    title = gettext("Q-Q plot"),
    dependencies = c("qqPlot", "qqPlotCi", "qqPlotCiLevel")
  )
  jaspResults[["qqPlot"]] <- plot

  plot$plotObject <- DistributionS7::plot_qq(
    distribution, variable, ci = options[["qqPlotCi"]], ci_level = options[["qqPlotCiLevel"]]
  )
}

ecdfPlot <- S7::new_generic("ecdPlot", "distribution")

S7::method(ecdfPlot, DistributionS7::DistributionContinuous) <- function(
    distribution, jaspResults, variable, options
) {
  if (!options[["ecdfPlot"]]) return()
  plot <- createJaspPlot(
    title = gettext("Empirical vs. Theoretical CDF"),
    dependencies = "ecdfPlot"
  )
  jaspResults[["ecdfPlot"]] <- plot

  plot$plotObject <- DistributionS7::plot_ecdf(distribution, variable, name = options[["variable"]])
}

ppPlot <- S7::new_generic("ppPlot", "distribution")

S7::method(ppPlot, DistributionS7::DistributionContinuous) <- function(
    distribution, jaspResults, variable, options
) {
  if (!options[["ppPlot"]]) return()
  plot <- createJaspPlot(
    title = gettext("P-P plot"),
    dependencies = c("ppPlot", "ppPlotCi", "ppPlotCiLevel")
  )
  jaspResults[["ppPlot"]] <- plot

  plot$plotObject <- DistributionS7::plot_pp(
    distribution, variable, ci = options[["ppPlotCi"]], ci_level = options[["ppPlotCiLevel"]]
  )
}


# Misc. ----
NormalDistribution <- function() {}

NormalDistributionInternal <- function(jaspResults, dataset, options, state=NULL) {
  distribution <- switch(
    options[["parametrization"]],
    sigma  = DistributionS7::normal(mu = options[["mu"]], sigma  = options[["sigma"]]),
    sigma2 = DistributionS7::normal(mu = options[["mu"]], sigma2 = options[["sigma2"]]),
    tau    = DistributionS7::normal(mu = options[["mu"]], tau    = options[["tau"]])
  )

  commonResults(distribution, jaspResults, dataset, options, state)
}

dependencies <- S7::new_generic("dependencies", "distribution")

S7::method(dependencies, DistributionS7::Normal) <- function(distribution) {
  c("parametrization", "mu", "sigma", "sigma2", "tau", "bessels_correction")
}
