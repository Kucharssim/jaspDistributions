jaspCommonDistribution <- function(distribution, jaspResults, dataset, options) {
  UseMethod("jaspCommonDistribution")
}

jaspCommonDistribution.jaspContinuousDistribution <- function(distribution, jaspResults, dataset, options) {
  jaspParametersSupportMoments(jaspResults, distribution, options)
}

jaspParametersSupportMoments <- function(jaspResults, distribution, options) {
  if(!isTRUE(options[["parsSupportMoments"]])) return()
  if(!is.null(jaspResults[["parsSupportMoments"]])) return()

  jaspResults[["parsSupportMoments"]] <- jaspBase::createJaspContainer(
    title = gettext("Parameters, Support, and Moments"), dependencies = "parsSupportMoments")

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
