# distributionR6 ----
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
    likelihood = function(log = TRUE) {
      ll <- sum(self$pdf(self$getData(), log = TRUE))

      if(log) return(ll)

      return(exp(ll))
    },
    mle = function(data = NULL) {
      if(!is.null(data)) self$setData(data)

      fn <- function(pars, data) {
        self$parameters$setValues(pars)

        -self$likelihood()
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
          private$errors$mle$vcov <- gettext("Error in estimating the variance-covariance matrix of the parameters; could not compute SE and Confidence intervals! <ul><li>Check outliers or feasibility of the distribution fitting the data.</li></ul>")
        }
      } else if (o$convergence != 0) {
        if(!is.null(o$errors)) {
          private$errors$mle$convergence <- gettextf("Optimization finished abnormally with code %s and the following message: %s. Interpret results with caution!", o$convergence, o$errors)
        } else {
          private$errors$mle$convergence <- gettextf("Optimization finished abnormally with code %s. Interpret results with caution!", o$convergence)
        }
      } else {
        self$parameters <- originalParameters
        private$errors$mle$fit <- gettext("Estimation failed: Optimization did not converge. <ul><li>Try adjusting parameter values, check outliers or feasibility of the distribution fitting the data.</li></ul>")
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
    errors = list(data = FALSE, mle = list()),
    data   = NULL
  )
)

# parSetR6 ----
parSetR6 <- R6::R6Class(
  "parSetR6",
  public = list(
    initialize = function(internal, primary, transformations = NULL) {
      names(internal) <- lapply(internal, \(p) p$name)
      private$.internal <- internal

      names(primary) <- lapply(primary, \(p) p$name)
      private$.primary <- primary

      private$.transformations <- transformations

      private$.transform()
    },
    summary = function(internal = TRUE, primary = TRUE) {
      int <- lapply(private$.internal, \(p) p$summary())
      int <- do.call(rbind, int)
      int[["type"]] <- "internal"

      pri <- lapply(private$.primary, \(p) p$summary())
      pri <- do.call(rbind, pri)
      pri[["type"]] <- "primary"

      if(internal && primary) {
        return(rbind(int, pri))
      } else if(internal) {
        return(int)
      } else if(primary) {
        return(pri)
      }
    }
  ),
  private = list(
    .internal = NULL,
    .primary = NULL,
    .transformations = NULL,
    .transform = function() {
      env <- list2env(self$primary)

      values <- lapply(private$.transformations, \(tt) {
        eval(expr = parse(text = tt), envir = env)
      })

      for(p in private$.internal)
        p$value <- values[[p$name]]
    }
  ),
  active = list(
    primary = function(values) {
      if(missing(values)) {
        values <- lapply(private$.primary, function(p) p$value)
        return(values)
      } else {
        values <- as.list(values)
        names  <- names(values)

        for(p in private$.primary) {
          if(p$name %in% names)
            p$value <- values[[p$name]]
        }

        private$.transform()
      }
    },
    free = function(values) {
      if(missing(values)) {
        values <- self$primary
        free   <- vapply(private$.primary, \(p) !p$fixed, logical(1))
        values <- values[free]
        return(values)
      } else {
        stop("Use $primary to set parameter values")
      }
    },
    fixed = function(values) {
      if(missing(values)) {
        values <- self$primary
        fixed  <- vapply(private$.primary, \(p) p$fixed, logical(1))
        values <- values[fixed]
        return(values)
      } else {
        stop("Cannot set fixed parameters")
      }
    },
    internal = function(values) {
      if(missing(values)) {
        values <- lapply(private$.internal, function(p) p$value)
      } else {
        stop("Cannot set internal parameters directly!")
      }
    }
  )
)


# parR6 ----
parR6 <- R6::R6Class(
  "parR6",
  public = list(
    initialize = function(name = "", label = name, value = 1, support = set6::Reals$new(), fixed = FALSE) {
      private$.support <- support
      self$name    <- name
      self$label   <- label

      private$.fixed <- fixed
      private$.checkValidValue(value)
      private$.value <- value
    },
    summary = function() {
      data.frame(name = self$name, label = self$label, value = private$.value, support = private$.support$strprint(), lower = private$.support$lower, upper = private$.support$upper, fixed = private$.fixed)
    },
    print = function() {
      print(self$summary())
      invisible(self)
    },
    name   = "",
    label  = ""
  ),
  private = list(
    .value = NULL,
    .support = set6::Set$new(),
    .fixed = FALSE,
    .checkValidValue = function(value) {
      stopifnot(all(private$.support$contains(value)))
    }
  ),
  active = list(
    value = function(value) {
      if(missing(value)) {
        return(private$.value)
      } else if (private$.fixed) {
        stop("Cannot re-set fixed parameter")
      } else{
        private$.checkValidValue(value)
        private$.value <- value
      }
    },
    support = function(support) {
      if(missing(support)) {
        return(private$.support)
      } else {
        stop("Cannot change support.")
      }
    },
    fixed = function(fixed) {
      if(missing(fixed)) {
        return(private$.fixed)
      } else {
        stop("Cannot fix or unfix a parameter")
      }
    }
  )
)
