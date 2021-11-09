#
# Copyright (C) 2013-2020 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

GenericDistribution <- function(jaspResults, dataset, options, state=NULL, distributionName){
  options <- .recodeCommonOptions(options)
  distribution <- .getDistribution(options, distributionName)

  .showDistribution(jaspResults, distribution, options)
  .simulateData    (jaspResults, distribution, options)
  .readAndCheckData(jaspResults, distribution, options, dataset)
  .descriptives    (jaspResults, distribution, options)
}

.recodeCommonOptions <- function(options) {
  options[["xRange"]] <- c(options[["min_x"]], options[["max_x"]])

  if(is.null(options[["highlightType"]])) {
    options$highlightInterval <- c(options[["min"]], options[["max"]])
  } else {
    options$highlightInterval <- switch(
      options[["highlightType"]],
      "minmax" = c(options[["min"]], options[["max"]]),
      "lower"  = c(options[["xRange"]][1], options[["lower_max"]]),
      "upper"  = c(options[["upper_min"]], options[["xRange"]][1])
    )
  }

  return(options)
}

#### Show distribution ----
.getDistribution <- function(options, distributionName) {

  distribution <- switch(
    distributionName,
    "Normal" = normalR6$new(mean = options[["mu"]], scale = options[["varValue"]], parametrization = options[["parametrization"]])
    )

  return(distribution)
}

.getPlotContainer <- function(jaspResults, distribution, options, name, title, position){
  if(!is.null(jaspResults[[name]])) return(jaspResults[[name]])

  plotsContainer <- createJaspContainer(title = gettext(title))
  plotsContainer$position <- position
  plotsContainer$dependOn(distribution$dependencies)

  jaspResults[[name]] <- plotsContainer

  return(plotsContainer)
}

.theoreticalPlotExplanation <- function(jaspContainer, distribution, options, optionName) {
  if(!isTRUE(options[["explanatoryText"]])) return()
  if(!is.null(jaspContainer[["explanation"]])) return()

  explanation <- createJaspHtml()
  explanation$dependOn(c(optionName, "explanatoryText"))
  explanation$position <- 1

  explanation[["text"]] <- distribution$explanation[[optionName]]
  jaspContainer[["explanation"]] <- explanation
}

.theoreticalPlot <- function(jaspContainer, distribution, options, optionName, title) {
  if(!is.null(jaspContainer[["plot"]])) return()

  plot <- createJaspPlot(title = title, width = 600, height = 320)
  plot$position <- 2 # after explanation, before formula
  plot$dependOn(c(optionName, "min_x", "max_x", "highlightType",
                  "highlightDensity", "highlightProbability",
                  "min", "max", "lower_max", "upper_min"))
  jaspContainer[["plot"]] <- plot

  plot$plotObject <- distribution[[optionName]](xRange = options$xRange,
                                                highlightProbability = options$highlightProbability,
                                                highlightDensity = options$highlightDensity,
                                                highlightInterval = options$highlightInterval)
}

.theoreticalPlotContainer <- function(jaspResults, distribution, options, optionName) {
  if(!isTRUE(options[[optionName]])) return()

  containerTitle <- switch(
    optionName,
    plotPDF = gettext("Probability Density Function"),
    plotPMF = gettext("Probability Mass Function"),
    plotCDF = gettext("Cumulative Distribution Function"),
    plotQF  = gettext("Quantile Function")
  )

  container <- .getPlotContainer(jaspResults, distribution, options, optionName, containerTitle, position = 3)
  .theoreticalPlotExplanation(container, distribution, options, optionName)

  plotTitle <- switch(
    optionName,
    plotPDF = gettext("Density Plot"),
    plotPMF = gettext("Probability Mass Plot"),
    plotCDF = gettext("Cumulative Probability Plot"),
    plotQF  = gettext("Quantile Plot")
  )
  .theoreticalPlot(container, distribution, options, optionName, plotTitle)
}

.showDistribution <- function(jaspResults, distribution, options) {
  .theoreticalPlotContainer(jaspResults, distribution, options, "plotPDF")
  .theoreticalPlotContainer(jaspResults, distribution, options, "plotPMF")
  .theoreticalPlotContainer(jaspResults, distribution, options, "plotCDF")
  .theoreticalPlotContainer(jaspResults, distribution, options, "plotQF")
}

# Simulate, read, and validate data ----
.simulateData <- function(jaspResults, distribution, options, sampleSizeName = "sampleSize"){
  if(!is.null(jaspResults[["simdata"]])) return()

  sample <- distribution$rng(options[[sampleSizeName]])

  jaspResults[['simdata']] <- createJaspState(sample)
  jaspResults[['simdata']]$dependOn(c("newVariableName", "simulateNow"))

  if (distribution$type == "continuous") {
    .setColumnDataAsScale  (options[["newVariableName"]], sample)
  } else if (distribution$type == "discrete") {
    .setColumnDataAsOrdinal(options[["newVariableName"]], sample)
  } else {
    .setColumnDataAsNominal(options[["newVariableName"]], sample)
  }

  return()
}

.checkInteger <- function(variable, errors){
  is_integer <- all((variable %% 1) == 0)

  if(isFALSE(errors) && is_integer){
    errors <- FALSE
  } else if(isFALSE(errors) && !is_integer){
    errors <- list(integer = TRUE, message = gettext("The following problem(s) occurred while running the analysis:<ul><li>Variable has to be discrete (i.e., integer)</li></ul>"))
  } else if(!is_integer){
    errors[['integer']] <- TRUE
    errors[['message']] <- paste(errors[['message']], gettext("<ul><li>Variable has to be discrete (i.e., integer)</li></ul>"))
  } else{
    errors <- errors
  }

  return(errors)
}

.readAndCheckData <- function(jaspResults, distribution, options, dataset) {
  if(options[["variable"]] == "") return()

  if(is.null(dataset) && distribution$type %in% c("continuous", "discrete")) {
    dataset <- .readDataSetToEnd(columns.as.numeric = options[["variable"]])
  } else if(is.null(dataset)) {
    dataset <- .readDataSetToEnd(columns.as.factor = options[["variable"]])
  }

  variable <- dataset[[.v(options[['variable']])]]
  variable <- variable[!is.na(variable)]

  distribution$setData(variable)
}

# Descriptives ----
.getDescriptivesContainer <- function(jaspResults, distribution, options) {
  if(!is.null(jaspResults[["descriptivesContainer"]])) return(jaspResults[["descriptivesContainer"]])

  container <- createJaspContainer(title = gettextf("Overview - %s", options[["variable"]]))
  container$position <- 6
  container$dependOn(c("variable", "simulateNow"))
  jaspResults[["descriptivesContainer"]] <- container


  return(container)
}

.descriptives <- function(jaspResults, distribution, options) {
  container <- .getDescriptivesContainer(jaspResults, distribution, options)

  if(!isFALSE(distribution$getErrors()$data)) {
    container$setError(distribution$getErrors()$data)
    return()
  }

  if(distribution$type %in% c("continuous", "discrete")) {
    .descriptivesContinuousMainTable (container, distribution, options)
    .descriptivesObservedMomentsTable(container, distribution, options)
  }
}

.descriptivesContinuousMainTable <- function(container, distribution, options) {
  if(!options$summary) return()
  if(!is.null(container[["descriptives"]])) return()
  if(is.null(distribution$getData())) return()

  descriptivesTable <- createJaspTable(title = gettext("Descriptives"))
  descriptivesTable$position <- 1
  descriptivesTable$dependOn(c("summary"))

  descriptivesTable$addColumnInfo(name = "variable",   title = gettext("Variable"),         type = "string")
  descriptivesTable$addColumnInfo(name = "sampleSize", title = gettext("n"),                type = "integer")
  descriptivesTable$addColumnInfo(name = "mean",       title = gettext("Mean"),             type = "number")
  descriptivesTable$addColumnInfo(name = "var",        title = gettext("Variance"),         type = "number")
  descriptivesTable$addColumnInfo(name = "sd",         title = gettext("Std. deviation"),   type = "number")
  descriptivesTable$addColumnInfo(name = "min",        title = gettext("Minimum"),          type = "number")
  descriptivesTable$addColumnInfo(name = "quantile25", title = gettextf("25%% Quantile"),   type = "number")
  descriptivesTable$addColumnInfo(name = "median",     title = gettext("Median"),           type = "number")
  descriptivesTable$addColumnInfo(name = "quantile75", title = gettextf("75%% Quantile"),   type = "number")
  descriptivesTable$addColumnInfo(name = "max",        title = gettext("Maximum"),          type = "number")

  container[["descriptives"]] <- descriptivesTable

  variable <- distribution$getData()
  descriptivesTable$addRows(list(variable   = options[['variable']],
                                 sampleSize = sum     (!is.na(variable)),
                                 mean       = mean    (variable,       na.rm = TRUE),
                                 var        = var     (variable,       na.rm = TRUE),
                                 sd         = sd      (variable,       na.rm = TRUE),
                                 min        = min     (variable,       na.rm = TRUE),
                                 quantile25 = quantile(variable, 0.25, na.rm = TRUE),
                                 median     = median  (variable, na.rm = TRUE),
                                 quantile75 = quantile(variable, 0.75, na.rm = TRUE),
                                 max        = max     (variable,       na.rm = TRUE)))

}

.descriptivesObservedMomentsTable <- function(container, distribution, options) {
  if(!options[["moments"]]) return()
  if(!is.null(container[["moments"]])) return()

  momentsTable <- createJaspTable(title = gettext("Observed Moments"))
  momentsTable$position <- 2
  momentsTable$dependOn(c("moments", "momentsUpTo"))

  momentsTable$addColumnInfo(name = "moment", title = gettext("Moment"), type = "integer")
  momentsTable$addColumnInfo(name = "raw",    title = gettext("Raw"),    type = "number")
  momentsTable$addColumnInfo(name = "central",title = gettext("Central"),type = "number")

  momentsTable$setExpectedSize(rows = options[["momentsUpTo"]])

  container[['moments']] <- momentsTable

  variable <- distribution$getData()
  if(is.null(variable)) return()

  res <- data.frame(moment = 1:options$momentsUpTo, raw = NA, central = NA)

  res$raw     <- .computeObservedMoments(variable, max.moment = options$momentsUpTo, about.mean = FALSE)
  res$central <- .computeObservedMoments(variable, max.moment = options$momentsUpTo, about.mean = TRUE)

  momentsTable$setData(res)
}
# .ldDescriptives <- function(jaspResults, variable, options, ready, errors, as = c("continuous", "discrete", "factor")){
#   as <- match.arg(as)
#   dataContainer <- .ldGetDataContainer(jaspResults, options, errors)
#
#   if(as == "continuous") {
#     ready <- ready && (isFALSE(errors) || (is.null(errors$infinity) && is.null(errors$observations)))
#     .ldSummaryContinuousTableMain(dataContainer, variable, options, ready)
#     .ldObservedMomentsTableMain  (dataContainer, variable, options, ready)
#     .ldPlotHistogram             (dataContainer, variable, options, ready)
#     .ldPlotECDF                  (dataContainer, variable, options, ready)
#   } else if(as == "discrete") {
#     ready <- ready && (isFALSE(errors) || (is.null(errors$infinity) && is.null(errors$observations)))
#     .ldSummaryContinuousTableMain(dataContainer, variable, options, ready)
#     .ldObservedMomentsTableMain  (dataContainer, variable, options, ready)
#     .ldPlotHistogram             (dataContainer, variable, options, ready, "discrete")
#     .ldPlotECDF                  (dataContainer, variable, options, ready)
#   } else {
#     ready <- ready && isFALSE(errors)
#     .ldSummaryFactorTableMain    (dataContainer, variable, options, ready)
#     .ldPlotHistogram             (dataContainer, variable, options, ready, "factor")
#   }
# }
