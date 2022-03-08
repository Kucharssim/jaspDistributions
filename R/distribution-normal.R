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
  save(options, file = "~/Downloads/opts.Rdata")
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

      self$parameters <- parSetR6$new(
        internal = list(
          parR6$new(name = "mean", support = set6::Reals$new()   ),
          parR6$new(name = "sd",   support = set6::PosReals$new())
        ),
        primary = switch(
          parametrization,
          "sigma" = list(
            parR6$new(name = "mu",    label = "\\mu",    value = mean,  support = set6::Reals$new()   ),
            parR6$new(name = "sigma", label = "\\sigma", value = scale, support = set6::PosReals$new())
          ),
          "sigma2" = list(
            parR6$new(name = "mu",     label = "\\mu",      value = mean,  support = set6::Reals$new()   ),
            parR6$new(name = "sigma2", label = "\\sigma^2", value = scale, support = set6::PosReals$new())
          ),
          "tau" = list(
            parR6$new(name = "mu",    label = "\\mu",  value = mean,  support = set6::Reals$new()   ),
            parR6$new(name = "tau",   label = "\\tau", value = scale, support = set6::PosReals$new())
          ),
          "kappa" = list(
            parR6$new(name = "mu",    label = "\\mu",    value = mean,  support = set6::Reals$new()   ),
            parR6$new(name = "kappa", label = "\\kappa", value = scale, support = set6::PosReals$new())
          )
        ),
        transformations = switch(
          parametrization,
          "sigma"  = list(mean = "mu", sd = "sigma"),
          "sigma2" = list(mean = "mu", sd = "sqrt(sigma2)"),
          "tau"    = list(mean = "mu", sd = "1/sqrt(tau)"),
          "kappa"  = list(mean = "mu", sd = "1/kappa")
        )
      )
    }
  )
)
