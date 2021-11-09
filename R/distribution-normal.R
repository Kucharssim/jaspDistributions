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

Normal <- function(jaspResults, dataset, options, state=NULL) {
  GenericDistribution(jaspResults, dataset, options, state, "Normal")
}

normalR6 <- R6::R6Class(
  classname = "normalR6",
  inherit = distributionR6,
  public = list(
    initialize = function(mean = 0, scale = 1, parametrization = c("sigma", "sigma2", "tau", "kappa")) {
      parametrization <- match.arg(parametrization)
      self$name <- gettext("normal distribution")
      self$dependencies <- c("mu", "varValue", "parametrization")
      private$pdfFun <- dnorm
      private$cdfFun <- pnorm
      private$qfFun  <- qnorm
      private$rngFun <- rnorm

      sd <- switch(parametrization,
                   "sigma"  = scale,
                   "sigma2" = sqrt(scale),
                   "tau"    = 1/sqrt(scale),
                   "kappa"  = 1/scale)

      self$parameters <- parameterCollectionR6$new(
        mean = parameterR6$new(value = mean, lower = -Inf, upper = Inf),
        sd   = parameterR6$new(value = sd,   lower = 0,    upper = Inf),
        transformations = c(mu = "mean", sigma2 = "sd^2", sigma = "sd", tau = "1/sd^2", kappa = "1/sd")
      )
    }
  )
)
