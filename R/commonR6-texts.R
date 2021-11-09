distributionR6$set(
  which = "public", name = "explanation",
  value = list(
    plotPDF = gettext("The probability density function (PDF), usually denoted as f(x), is a function of a random variable X.
    The value of f(x) provides the relative likelihood that a realization of the random variable X yields a value equal to x.
    More formally, the PDF is defined as a derivative of a cumulative distribution function (CDF).

    The density plot displays the probability density function of a random variable.
    The <i>y</i>-axis displays the value of the density function for a particular value of the random variable (displayed on the <i>x</i>-axis)."),

    plotPMF = gettext("The probability mass function (PMF), usually denoted as f(x), is a function of a random variable X.
    The value of f(x) provides the probability that a realization of the random variable X yields a value equal to x.

    The probability mass plot displays the probability mass function of a random variable.
    The <i>y</i>-axis displays the value of the probability mass function for a particular value of the random variable (displayed on the <i>x</i>-axis)."),

    plotCDF = gettext("The cumulative distribution function (CDF), usually denoted as F(x), is a function of a random variable X.
    The value of F(x) provides the probability that a realization of the random variable X yields a value that is equal to or smaller than x.

    The cumulative probability plot displays the cumulative distribution function of a random variable.
    The <i>y</i>-axis displays the value of the cumulative distribution function for a particular value of the random variable (displayed on the <i>x</i>-axis)."),

    plotQF  = gettext("The quantile function, usually denoted as Q(p), is the inverse of the cumulative distribution function.
    The function gives the quantile such that the probability of the random variable being less than or equal to that value equals the given probability p.

    The quantile plot displays the quantile function.
    The <i>y</i>-axis displays the quantile of which the probability that the random variable is less or equal to that value is equal to p (displayed on the <i>x</i>-axis).")
  )
)

#     intro = gettextf("<h3> Demonstration of the %1$s </h3>
# This demonstration is divided into four parts.
# The first part displays the %1$s, its probability density function, cumulative distribution function, and quantile function.
# The second part allows you to generate data from the %1$s, compute descriptive statistics, and display descriptive plots.
# The third part allows you to estimate the parameters of the %1$s.
# The fourth part allows you to check the fit of the %1$s to the data.
#
# <b>References</b>
#
# Blitzstein, J. K., & Hwang, J. (2014). <i>Introduction to probability.</i> Chapman and Hall/CRC.
#
# Leemis, L. M., & Pasupathy, R. (2019). The ties that bind. <i>Significance, 16</i>(4), 8–9.
#
# For relationships with other distributions, visit www.math.wm.edu/~leemis/chart/UDR/UDR.html.
#
# https://en.wikipedia.org/wiki/List_of_probability_distributions", self$name)
