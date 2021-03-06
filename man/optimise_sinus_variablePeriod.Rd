% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optimise_sinus.R
\name{optimise_sinus_variablePeriod}
\alias{optimise_sinus_variablePeriod}
\title{Optimise Sinus Fit for Variable Period}
\usage{
optimise_sinus_variablePeriod(
  temp_df,
  optFunc = opt_func,
  opt_limits = c(100, 500),
  opt_tolerance = 0.001,
  opt_debug = FALSE
)
}
\arguments{
\item{temp_df}{data frame with temperature data and columns "date"
(YYYY-MM-DD) and "value"}

\item{optFunc}{optimisation function (default: \code{\link{opt_func}} )}

\item{opt_limits}{optimisation limits for "period_length" (default:
c(100,500))}

\item{opt_tolerance}{(default: 0.001)}

\item{opt_debug}{show debug information (default: FALSE)}
}
\value{
list with fit parameters ("paras"), goodness-of-fit values ("gof"),
special points, i.e. min/max/turning-points ("points), fit model
("lm_model") and input data ("data")
}
\description{
Optimise Sinus Fit for Variable Period
}
\references{
https://stats.stackexchange.com/questions/77543/how-do-i-get-the-amplitude-and-phase-for-sine-wave-from-lm-summary
}
