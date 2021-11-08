distributionR6 <- R6::R6Class(
  "distributionR6",
  public = list(
    name = NULL,
    parameters = NULL,
    support = c(lower = -Inf, upper = Inf),
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
    fit = function(data) {
      fn <- function(pars, data) {
        self$parameters$setValues(pars)

        -self$likelihood(data)
      }
      originalParameters <- self$parameters$clone(deep = FALSE)
      o <- try(optim(par = self$parameters$getValues(), fn = fn, method = "L-BFGS-B",
                 lower = self$parameters$getLower(), upper = self$parameters$getUpper(),
                 hessian = TRUE, data = data))

      if (!inherits(o, "try-error")) {
        vcov <- try(solve(o$hessian))
        if(!inherits(vcov, "try-error")) {
          self$parameters$vcov <- vcov
        } else {
          private$message$vcov <- gettext("Error in estimating the variance-covariance matrix of the parameters; could not compute SE and Confidence intervals! <ul><li>Check outliers or feasibility of the distribution fitting the data.</li></ul>")
        }
      } else if (o$convergence != 0) {
        if(!is.null(o$message)) {
          private$message$convergence <- gettextf("Optimization finished abnormally with code %s and the following message: %s. Interpret results with caution!", o$convergence, o$message)
        } else {
          private$message$convergence <- gettextf("Optimization finished abnormally with code %s. Interpret results with caution!", o$convergence)
        }
      } else {
        self$parameters <- originalParameters
        private$message$fit <- gettext("Estimation failed: Optimization did not converge. <ul><li>Try adjusting parameter values, check outliers or feasibility of the distribution fitting the data.</li></ul>")
      }

      invisible(self)
    }
  ),
  private = list(
    pdfFun = NULL,
    cdfFun = NULL,
    qfFun  = NULL,
    rngFun = NULL,
    message = list()
  )
)

normalR6 <- R6::R6Class(
  "normalR6",
  inherit = distributionR6,
  public = list(
    initialize = function(options) {
      self$name <- gettext("normal distribution")
      private$pdfFun <- dnorm
      private$cdfFun <- pnorm
      private$qfFun  <- qnorm
      private$rngFun <- rnorm

      self$parameters <- parameterCollectionR6$new(
        mean = parameterR6$new(value = 0, lower = -Inf, upper = Inf),
        sd   = parameterR6$new(value = 1, lower = 0,    upper = Inf),
        transformations = c(mu = "mean", sigma2 = "sd^2", sigma = "sd", tau = "1/sd^2", kappa = "1/sd")
      )
    }
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
