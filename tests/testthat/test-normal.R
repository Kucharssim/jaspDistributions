
devtools::install_github("Kucharssim/DistributionS7")

options <- jaspTools::analysisOptions("NormalDistribution")
options[["explanatoryText"]] <- TRUE
options[["pdfPlot"]] <- TRUE
options[["cdfPlot"]] <- TRUE
options[["qfPlot"]]  <- TRUE
options[["distributionTable"]] <- TRUE
options[["distributionTableRangeBasedOn"]] <- "cdf"
options[["distributionTableMin"]] <- 0.1
options[["distributionTableMax"]] <-  0.9
options[["distributionTableLength"]] <- 9

options[["parameterTable"]] <- TRUE
options[["variable"]] <- "contNormal"

options[["dataDescriptives"]] <- TRUE
options[["dataHistogram"]] <- TRUE
options[["estimateParameters"]] <- TRUE
options[["parameterTableUncertainty"]] <- TRUE


options[["histPlot"]] <- TRUE
options[["qqPlot"]] <- TRUE
options[["ecdfPlot"]] <- TRUE
options[["ppPlot"]] <- TRUE

options[["goodnessOfFit"]] <- TRUE
options[["goodnessOfFitBootstrap"]] <- FALSE
options[["goodnessOfFitBootstrapSamples"]] <- 100

options[["informationCriteria"]] <- TRUE
options[["momentsTable"]] <- TRUE
options[["momentsTableNumber"]] <- 6


results <- jaspTools::runAnalysis("NormalDistribution", dataset="debug.csv", options=options)
