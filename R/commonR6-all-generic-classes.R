distributionR6 <- R6::R6Class(
  classname = "distributionR6",
  public = list(
    name = NULL,
    parameters = NULL,
    support = c(lower = -Inf, upper = Inf),
    type = "continuous",
    dependencies = NULL,
    pdf = function(x, log = FALSE) {
      args <- as.list(self$parameters$getValues())
      args[["x"]]   <- x
      args[["log"]] <- log

      return(do.call(private$pdfFun, args))
    },
    cdf = function(q, lower.tail = TRUE, log.p = FALSE) {
      args <- as.list(self$parameters$getValues())
      args[["q"]] <- q
      args[["lower.tail"]] <- lower.tail
      args[["log.p"]] <- log.p

      return(do.call(private$cdfFun, args))
    },
    qf  = function(p, lower.tail = TRUE, log.p = FALSE) {
      args <- as.list(self$parameters$getValues())
      args[["p"]] <- p
      args[["lower.tail"]] <- lower.tail
      args[["log.p"]] <- log.p

      return(do.call(private$qfFun, args))
    },
    rng = function(n) {
      args <- as.list(self$parameters$getValues())
      args[["n"]] <- n
      do.call(private$rngFun, args)
    },
    likelihood = function(x, log = TRUE) {
      ll <- sum(self$pdf(x, log = TRUE))

      if(log) return(ll)

      return(exp(ll))
    },
    fit = function(data = NULL) {
      if(!is.null(data)) self$setData(data)

      fn <- function(pars, data) {
        self$parameters$setValues(pars)

        -self$likelihood(data)
      }
      originalParameters <- self$parameters$clone(deep = FALSE)
      o <- try(optim(par = self$parameters$getValues(), fn = fn, method = "L-BFGS-B",
                 lower = self$parameters$getLower(), upper = self$parameters$getUpper(),
                 hessian = TRUE, data = self$data))

      if (!inherits(o, "try-error")) {
        vcov <- try(solve(o$hessian))
        if(!inherits(vcov, "try-error")) {
          self$parameters$vcov <- vcov
        } else {
          private$errors$vcov <- gettext("Error in estimating the variance-covariance matrix of the parameters; could not compute SE and Confidence intervals! <ul><li>Check outliers or feasibility of the distribution fitting the data.</li></ul>")
        }
      } else if (o$convergence != 0) {
        if(!is.null(o$errors)) {
          private$errors$convergence <- gettextf("Optimization finished abnormally with code %s and the following message: %s. Interpret results with caution!", o$convergence, o$errors)
        } else {
          private$errors$convergence <- gettextf("Optimization finished abnormally with code %s. Interpret results with caution!", o$convergence)
        }
      } else {
        self$parameters <- originalParameters
        private$errors$fit <- gettext("Estimation failed: Optimization did not converge. <ul><li>Try adjusting parameter values, check outliers or feasibility of the distribution fitting the data.</li></ul>")
      }

      invisible(self)
    },
    setData = function(data) {
      validData <- self$checkData(data)
      if(validData) {
        private$data <- data
      } else {
        warning(gettext("Invalid data!"))
      }

      invisible(self)
    },
    getData = function(data) {
      return(private$data)
    },
    checkData = function(data) {
      errors <- .hasErrors(dataset = data.frame(variable = data),
                           type    = c("infinity", "variance", "observations", "limits"),
                           observations.amount = sprintf("<%s", 2),
                           limits.min = self[["support"]][["lower"]],
                           limits.max = self[["support"]][["upper"]],
                           exitAnalysisIfErrors = FALSE)

      if(isFALSE(errors)) {
        return(TRUE)
      } else {
        private$errors$data <- errors$message
        return(FALSE)
      }
    },
    getErrors = function() {
      return(private$errors)
    }
  ),
  private = list(
    pdfFun = NULL,
    cdfFun = NULL,
    qfFun  = NULL,
    rngFun = NULL,
    errors = list(data = FALSE, fit = FALSE),
    data   = NULL
  )
)

parameterCollectionR6 <- R6::R6Class(
  "parameterCollectionR6",
  public = list(
    parameterList = list(),
    transformations = NULL,
    vcov = NULL,
    initialize = function(..., transformations = NULL) {
      self$parameterList <- list(...)

      if(is.null(transformations)) {
        self$transformations <- self$getNames()
        names(self$transformations) <- self$getNames()
      } else {
        self$transformations <- transformations
      }
    },
    getNames = function() {
      names(self$parameterList)
    },
    getValues = function() {
      out <- sapply(self$parameterList, function(p) p$getValue())
      names(out) <- self$getNames()
      return(out)
    },
    getLower = function() {
      out <- sapply(self$parameterList, function(p) p$getLower())
      names(out) <- self$getNames()
      return(out)
    },
    getUpper = function() {
      out <- sapply(self$parameterList, function(p) p$getUpper())
      names(out) <- self$getNames()
      return(out)
    },
    setValues = function(values) {
      nms <- names(values)
      for(i in seq_along(values)) {
        self$parameterList[[nms[i]]]$setValue(values[[i]])
      }

      invisible(self)
    },
    summary = function(ciLevel = 0.95) {
      result <-
        sapply(self$transformations,
               function(tr) car::deltaMethod(self$getValues(), tr, self$vcov, level = ciLevel))
      result <- t(result)
      result <- as.data.frame(result)
      colnames(result) <- c("estimate", "se", "lower", "upper")

      result <- data.frame(
        label = rownames(result),
        result
      )

      return(result)
    }
  )
)

parameterR6 <- R6::R6Class(
  "parameterR6",
  public = list(
    initialize = function(value = 1, lower = -Inf, upper = Inf) {
      stopifnot(lower < upper)

      private$lower <- lower
      private$upper <- upper

      private$checkValidValue(value)
      private$value <- value
    },
    setValue = function(value) {
      private$checkValidValue(value)
      private$value <- value

      invisible(self)
    },
    getValue = function() {
      return(private$value)
    },
    getLower = function() {
      return(private$lower)
    },
    getUpper = function() {
      return(private$upper)
    }
  ),
  private = list(
    value = NULL,
    lower = -Inf,
    upper = Inf,
    checkValidValue = function(value) {
      stopifnot(private$lower < value)
      stopifnot(private$upper > value)
    }
  )
)
