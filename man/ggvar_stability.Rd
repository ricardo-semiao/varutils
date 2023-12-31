% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggvar_stability.R
\name{ggvar_stability}
\alias{ggvar_stability}
\title{Plot for Structural Stability of a VAR}
\usage{
ggvar_stability(
  x,
  series = NULL,
  ci = 0.95,
  ...,
  args_line = list(),
  args_hline = list(linetype = 2, color = "blue"),
  args_facet = list()
)
}
\arguments{
\item{x}{A "varest" object to pass to \link[vars]{stability}, or, directly, a
"varstabil" object.}

\item{series}{A character vector with variables (column names) to consider. Defaults to all (\code{NULL}).}

\item{ci}{The level of confidence for the \link[strucchange]{boundary}.}

\item{...}{Further arguments passed to \link[strucchange]{boundary}.}

\item{args_line}{Aditional arguments passed to \link[ggplot2]{geom_line}.}

\item{args_hline}{Aditional arguments passed to \link[ggplot2]{geom_hline}.}

\item{args_facet}{Aditional arguments passed to \link[ggplot2]{facet_wrap}.}
}
\value{
An object of class \code{ggplot}.
}
\description{
Plots the result of a \link[vars]{stability} call. Confidence intevals are
 calculated using \link[strucchange]{boundary}.
}
\examples{
ggvar_stability(vars::VAR(freeny[-2]))

}
